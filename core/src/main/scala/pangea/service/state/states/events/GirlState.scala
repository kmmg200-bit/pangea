package pangea.service.state.states.events

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.TreasureMapGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{BrewKind, MaterialKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.schedule.TaskKind
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.barrel.BarrelRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.schedule.Scheduler
import pangea.repository.bank.BankRepository
import pangea.service.purse.Purse
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.events.GirlState._
import pangea.service.state.states.tavern.GirlDeal
import pangea.service.state.{AppButton, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Событие «Девушка» (2% в лабиринте, доля забрана у ручья). Девушка выбегает с
  * криком «Помогите!», следом — трое вооружённых одной расы. Дальше ветвится:
  * бой с тремя (первые три тира), благодарность или просьба проводить до
  * города; в городе — простая благодарность, карта клада отца или таверна, где
  * всё может кончиться уединением (травмы пройдут за полтора часа, кошелёк
  * полегчает), а может — братом с ножом и вопросом «серебро или жизнь».
  *
  * Весь ход события — в `scene_data` ([[GirlScene]]): шаг, раса бандитов, цена
  * карты, потерянное серебро, начало ожидания в комнате. Бои идут через обычный
  * [[StateType.Battle]] с маршрутом добычи обратно сюда (после бандитов) или в
  * город (после брата). */
case class GirlState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  barrelRepo:    BarrelRepository,
  scheduler:     Scheduler,
  content:       SceneContent,
  bank:          Option[BankRepository] = None
) extends State {

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "Help"          -> Target.Run { (u, _, r) => show(u, r, Step.Bandits) },
      "PassBy"        -> Target.Run { (u, _, r) => leave(u, r, "girl.leftAlone", StateType.Dungeon) },
      "Fight"         -> Target.Run { (u, _, r) => fightBandits(u, r) },
      "Apologize"     -> Target.Run { (u, _, r) => leave(u, r, "girl.leftAlone", StateType.Dungeon) },
      "Onward"        -> Target.Run { (u, _, _) => clear(u).as(StateType.Dungeon) },
      "Escort"        -> Target.Run { (u, _, r) => escort(u, r) },
      "Decline"       -> Target.Run { (u, _, r) => leave(u, r, "girl.leftAlone", StateType.Dungeon) },
      "Ask"           -> Target.Run { (u, _, r) => reward(u, r, _.lvl * RepPerLevelCity) *> askWhatSheDid(u, r) },
      "Farewell"      -> Target.Run { (u, _, r) => reward(u, r, _.lvl * RepPerLevelCity) *> clear(u).as(StateType.GlobalMap) },
      "FarewellAsked" -> Target.Run { (u, _, r) => rewardSpread(u, r, RepPerLevelFarewell) *> clear(u).as(StateType.GlobalMap) },
      "OfferBuy"      -> Target.Run { (u, _, r) => offerMap(u, r) },
      "WishLuck"      -> Target.Run { (u, _, r) => rewardSpread(u, r, RepPerLevelFarewell) *> clear(u).as(StateType.GlobalMap) },
      "BuyMap"        -> Target.Run { (u, _, r) => buyMap(u, r) },
      "CantAfford"    -> Target.Run { (u, _, r) => reward(u, r, _.lvl * RepPerLevelCity) *> clear(u).as(StateType.GlobalMap) },
      "ToTavern"      -> Target.Run { (u, _, r) => show(u, r, Step.Tavern) },
      "ToRoom"        -> Target.Run { (u, _, r) => show(u, r, Step.Room) },
      "LeaveTavern"   -> Target.Run { (u, _, r) => reward(u, r, _.lvl * RepPerLevelTavern) *> clear(u).as(StateType.GlobalMap) },
      "Intimacy"      -> Target.Run { (u, _, r) => intimacy(u, r) },
      "Excuse"        -> Target.Run { (u, _, r) => show(u, r, Step.Excuse) },
      "Refuse"        -> Target.Run { (u, _, r) => refuse(u, r) },
      "Explain"       -> Target.Run { (u, _, r) => demand(u, r) },
      // Отдать — сперва переспросить: серебра уйдёт много, а вернуть его нельзя.
      "PayBrother"    -> Target.Run { (u, _, r) => confirmPay(u, r) },
      "PayBrotherYes" -> Target.Run { (u, _, r) => payBrother(u, r) },
      "PayBrotherNo"  -> Target.Run { (u, _, r) => backToDemand(u, r) },
      "FightBrother"  -> Target.Run { (u, _, r) => fightBrother(u, r) },
      "LeaveRoom"     -> Target.Run { (u, _, r) => leaveRoom(u, r) }
    ),
    // Любой другой ввод — снова текущий шаг: в темноте это её экран (см. lightsOut).
    fallback = Target.Run { (u, _, r) => resume(u, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.GlobalMap, StateType.Battle, StateType.Girl)

  /** Вход: с чистой сценой — встреча; после боя с бандитами (добыча вернула
    * сюда) — что девушка скажет; иначе — продолжить с текущего шага. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    readScene(user).flatMap {
      case None =>
        for {
          idx  <- Random.nextIntBounded(Race.mortals.size)
          race  = Race.mortals(idx)
          _    <- writeScene(user, GirlScene(Step.Meet, race.entryName))
          _    <- renderer.show(user, Screen(
                    content.format("girl.meet.text", "race" -> race.genitivePlural),
                    content.screen("girl.meet").choices))
        } yield ()
      case Some(scene) if scene.step == Step.AfterFight => afterFight(user, renderer, scene)
      case Some(scene) if scene.step == Step.RoomWait   => roomWait(user, renderer, scene)
      case Some(scene) => renderer.show(user, screenFor(scene))
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Лабиринт ────────────────────────────────────────────────────────────────

  /** Бой с тремя бандитами: раса из сцены, редкость каждого — из первых трёх
    * тиров. Добыча вернёт сюда, на шаг после боя. */
  private def fightBandits(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      scene <- requireScene(user)
      race   = Race.withName(scene.race)
      tiers <- ZIO.foreach(List.fill(BanditCount)(()))(_ => Random.nextIntBounded(BanditTiers.size).map(BanditTiers))
      mobs   = tiers.map(MonsterGenerator.generateOfRaceAndRarity(hero.dungeonLevel, race, _))
      energies <- ZIO.foreach(mobs)(m =>
                    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
      routing = LootData(Nil, Nil, returnState = Some(StateType.Girl),
                  eventData = Some(scene.copy(step = Step.AfterFight).asJson))
      // Бандиты в лабиринте — с отрядом, как любой бой здесь.
      _ <- heroDao.writeActiveBattle(user.userId, SoloPveBattle.fromGroup(mobs, hero, energies).asJson)
      _ <- heroDao.writeSceneData(user.userId, routing.asJson)
      _ <- renderer.show(user, Screen(content.format("girl.fightStart", "race" -> race.genitivePlural), Nil))
    } yield StateType.Battle

  /** После победы: либо благодарность серебром и репутацией, либо просьба
    * проводить до города — поровну. */
  private def afterFight(user: User, renderer: Renderer, scene: GirlScene): Task[Unit] =
    Random.nextIntBounded(2).flatMap {
      case 0 =>
        for {
          hero   <- getHero(user)
          spread <- Random.nextLongBetween(SpreadMin, SpreadMax + 1L)
          silver  = (hero.lvl * SilverPerLevel * spread / 100L * Achievement.silverPct(hero) / 100L).max(1L)
          rep     = hero.lvl * RepPerLevelThanks
          _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
          _      <- heroDao.updateGuildReputation(user.userId, hero.guildReputation + rep)
          _      <- writeScene(user, scene.copy(step = Step.Thanks))
          _      <- renderer.show(user, Screen(
                      content.text("girl.thanks.text") + "\n\n" +
                        content.format("girl.thanksReward", "silver" -> silver.toString, "rep" -> rep.toString),
                      content.screen("girl.thanks").choices))
        } yield ()
      case _ =>
        writeScene(user, scene.copy(step = Step.Escort)) *> renderer.show(user, content.screen("girl.escort"))
    }

  /** Проводить до города: три исхода — 48% благодарность, 2% карта отца, 48%
    * просьба отвести в таверну. */
  private def escort(user: User, renderer: Renderer): Task[StateType] =
    for {
      roll <- Random.nextIntBounded(100)
      step  = if (roll < CityThanksPct) Step.CityThanks
              else if (roll < CityThanksPct + CityMapPct) Step.CityMap
              else Step.CityTavern
      _    <- show(user, renderer, step, content.text("girl.escorted"))
    } yield StateType.Girl

  // ── Город: карта отца ───────────────────────────────────────────────────────

  private def offerMap(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      spread <- Random.nextLongBetween(SpreadMin, SpreadMax + 1L)
      price   = (hero.lvl * MapPricePerLevel * spread / 100L).max(1L)
      scene  <- requireScene(user)
      _      <- writeScene(user, scene.copy(step = Step.MapOffer, price = price))
      _      <- renderer.show(user, Screen(
                  content.format("girl.mapOffer.text", "price" -> price.toString),
                  content.screen("girl.mapOffer").choices))
    } yield StateType.Girl

  private def buyMap(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      scene <- requireScene(user)
      price  = scene.price
      wallet <- purse.wallet(hero)
      // Серебра мало — она подождёт в таверне по той же цене (см. GirlDeal).
      res <- if (!wallet.canAfford(price))
               nowMs.flatMap(now => GirlDeal.save(heroDao, user.userId, GirlDeal(price, hero.lvl, now + GirlDeal.WaitMs))) *>
                 renderer.show(user, Screen(content.format("girl.mapWait", "price" -> price.toString), Nil)) *>
                 clear(user).as(StateType.GlobalMap)
             else
               for {
                 map   <- itemRepo.persist(hero.id, TreasureMapGenerator.create(hero.lvl, half = false))
                 added <- inventoryRepo.addItem(hero.id, map).as(true).catchAll(_ => ZIO.succeed(false))
                 next  <- if (!added)
                            renderer.show(user, screenFor(scene).noted(content.text("girl.mapNoRoom"))).as(StateType.Girl)
                          else
                            purse.charge(user.userId, hero, price) *>
                              renderer.show(user, Screen(content.format("girl.mapBought", "price" -> price.toString), Nil)) *>
                              clear(user).as(StateType.GlobalMap)
               } yield next
    } yield res

  // ── Таверна: комната ────────────────────────────────────────────────────────

  /** Уединение: кошелёк худеет на 25–50% (скажем в конце), травмы пройдут через
    * полтора часа, свет гаснет на полчаса (см. [[lightsOut]]). Уже уединялись —
    * значит, герой вернулся от уговоров («Ну раз такое дело…» после «Прошу
    * прощения…» в темноте): снова темнота, без второго уединения и второй
    * пропажи. */
  private def intimacy(user: User, renderer: Renderer): Task[StateType] =
    requireScene(user).flatMap { scene =>
      if (scene.roomStartedAt.isDefined) writeScene(user, scene.copy(step = Step.RoomWait)) *> leaveRoom(user, renderer)
      else
        for {
          now   <- nowMs
          hero  <- getHero(user)
          pct   <- Random.nextLongBetween(LossPctMin, LossPctMax + 1L)
          lost   = hero.silver * pct / 100L
          _     <- heroDao.updateSilver(user.userId, hero.silver - lost)
          healAt = now + HealDelayMs
          _     <- ZIO.when(hero.traumaActive(now) && hero.traumaUntil.exists(_ > healAt))(
                     heroDao.updateTrauma(user.userId, Some(healAt), hero.traumaNames))
          _     <- writeScene(user, scene.copy(step = Step.RoomWait, lost = lost, roomStartedAt = Some(now)))
          _     <- scheduler.schedule(user.userId, now + RoomWaitMs, TaskKind.GirlRoom, StateType.Girl, LeaveRoomAction)
          _     <- renderer.show(user, lightsOut.noted(content.text("girl.intimacy")))
        } yield StateType.Girl
    }

  /** Тёмная комната. Через полчаса герой уходит сам (таймер шлёт «LeaveRoom»),
    * а раньше — только через «Прошу прощения, но у меня есть дела»: та же
    * кнопка, что в комнате, и тот же путь — уговоры, а настоит — брат. Так
    * случайно не уйдёшь. Рядом кнопка приложения. Без своих кнопок ВК оставлял
    * здесь клавиатуру комнаты — и её «Оуу, ну раз такое дело…». */
  private def lightsOut: Screen =
    Screen(content.text("girl.lightsOut"),
      AppButton.inRow(content, List(content.choice("Excuse", "girl.room.choices.Excuse"))))

  /** Пока полчаса не прошли — тот же тёмный экран; прошли — выход из комнаты. */
  private def roomWait(user: User, renderer: Renderer, scene: GirlScene): Task[Unit] =
    nowMs.flatMap { now =>
      if (scene.roomStartedAt.exists(start => now - start >= RoomWaitMs)) leaveRoom(user, renderer).unit
      else renderer.show(user, lightsOut)
    }

  /** Полчаса прошли — герой выходит из комнаты. Раньше срока — та же темнота.
    * Ушёл от неё к уговорам или к брату — таймер их не прерывает: вернётся в
    * темноту — выйдет там (см. [[intimacy]]). */
  private def leaveRoom(user: User, renderer: Renderer): Task[StateType] =
    for {
      now   <- nowMs
      scene <- requireScene(user)
      done   = scene.roomStartedAt.exists(start => now - start >= RoomWaitMs)
      res   <- if (scene.step != Step.RoomWait) ZIO.succeed(StateType.Girl)
               else if (!done) renderer.show(user, lightsOut).as(StateType.Girl)
               else
                 scheduler.cancel(user.userId, TaskKind.GirlRoom) *>
                   renderer.show(user, Screen(Screen.joined(content.text("girl.roomLeft"),
                     content.format("girl.silverGone", "silver" -> scene.lost.toString)), Nil)) *>
                   clear(user).as(StateType.GlobalMap)
    } yield res

  /** «Нет, мне правда надо идти!» — она зовёт брата. Если уже уединялись (ушёл
    * из темноты раньше срока), таймер комнаты больше не нужен, а пропажу из
    * кошелька герой замечает, пока одевается: на улицу он выйдет уже после
    * брата. */
  private def refuse(user: User, renderer: Renderer): Task[StateType] =
    requireScene(user).flatMap { scene =>
      val noticed = Option.when(scene.roomStartedAt.isDefined)(
                      content.format("girl.silverNoticed", "silver" -> scene.lost.toString)).getOrElse("")
      ZIO.when(scene.roomStartedAt.isDefined)(scheduler.cancel(user.userId, TaskKind.GirlRoom)) *>
        show(user, renderer, Step.Brother, Screen.joined(noticed, content.text("girl.refuse")))
    }

  // ── Таверна: брат ───────────────────────────────────────────────────────────

  /** Брат требует половину всего серебра — с собой и в бочке. */
  private def demand(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      barrel <- barrelRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      half    = (hero.silver + barrel.silver) / 2L
      scene  <- requireScene(user)
      _      <- writeScene(user, scene.copy(step = Step.Demand, price = half))
      _      <- renderer.show(user, demandScreen(half))
    } yield StateType.Girl

  private def demandScreen(half: Long): Screen =
    Screen(content.format("girl.demand.text", "silver" -> half.toString), List(
      Choice("PayBrother", content.format("girl.payLabel", "silver" -> half.toString)),
      content.choice("FightBrother", "girl.fightLabel")
    ))

  /** «Отдать серебро» сперва переспрашивает: сколько уйдёт и откуда. Не во
    * время требования брата (кнопка из старого сообщения) — просто текущий шаг. */
  private def confirmPay(user: User, renderer: Renderer): Task[StateType] =
    requireScene(user).flatMap { scene =>
      if (scene.step == Step.Demand || scene.step == Step.PayConfirm) show(user, renderer, Step.PayConfirm)
      else renderer.show(user, screenFor(scene)).as(StateType.Girl)
    }

  /** Передумал отдавать — снова требование брата: отдать или взять оружие. */
  private def backToDemand(user: User, renderer: Renderer): Task[StateType] =
    requireScene(user).flatMap { scene =>
      if (scene.step == Step.PayConfirm) show(user, renderer, Step.Demand)
      else renderer.show(user, screenFor(scene)).as(StateType.Girl)
    }

  private def payConfirmScreen(half: Long): Screen =
    Screen(content.format("girl.payConfirm.text", "silver" -> half.toString), List(
      content.choice("PayBrotherYes", "girl.payConfirm.yes", "silver" -> half.toString),
      content.choice("PayBrotherNo", "girl.payConfirm.no")
    ))

  /** Отдать — только переспросив ([[confirmPay]]): сначала из кошелька,
    * остаток — из бочки. Не подтверждено (кнопка из старого сообщения) —
    * серебро цело, просто текущий шаг. */
  private def payBrother(user: User, renderer: Renderer): Task[StateType] =
    requireScene(user).flatMap { scene =>
      if (scene.step != Step.PayConfirm) renderer.show(user, screenFor(scene)).as(StateType.Girl)
      else for {
        hero    <- getHero(user)
        half     = scene.price
        fromHand = hero.silver.min(half)
        fromBarrel = half - fromHand
        _       <- heroDao.updateSilver(user.userId, hero.silver - fromHand)
        _       <- ZIO.when(fromBarrel > 0L)(
                     barrelRepo.withdrawSilver(hero.id, fromBarrel).mapError(e => new Throwable(e.toString)))
        _       <- renderer.show(user, Screen(content.format("girl.paid", "silver" -> half.toString), Nil))
        _       <- clear(user)
      } yield StateType.GlobalMap
    }

  /** Взять оружие: редкий человек уровня этажа, добыча — и в город. Драка в
    * таверне — без отряда: наёмники остались за столом. */
  private def fightBrother(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      mob   = MonsterGenerator.generateOfRaceAndRarity(hero.dungeonLevel, BrotherRace, BrotherRarity)
      pct  <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      _    <- heroDao.writeActiveBattle(user.userId, SoloPveBattle.from(mob, hero, squad = false).withStartEnergy(pct).asJson)
      _    <- heroDao.writeSceneData(user.userId, LootData(Nil, Nil, returnState = Some(StateType.GlobalMap)).asJson)
      _    <- renderer.show(user, Screen(content.text("girl.brotherFight"), Nil))
    } yield StateType.Battle

  // ── Общее ───────────────────────────────────────────────────────────────────

  /** Показать шаг и запомнить его. `note` — что было перед ним: тем же
    * сообщением, над шагом. */
  private def show(user: User, renderer: Renderer, step: String, note: String = ""): Task[StateType] =
    for {
      scene <- requireScene(user)
      next   = scene.copy(step = step)
      _     <- writeScene(user, next)
      _     <- renderer.show(user, screenFor(next).noted(note))
    } yield StateType.Girl

  private def screenFor(scene: GirlScene): Screen = scene.step match {
    case Step.Meet     => Screen(content.format("girl.meet.text", "race" -> Race.withName(scene.race).genitivePlural), content.screen("girl.meet").choices)
    case Step.Bandits  => content.screen("girl.bandits")
    case Step.Thanks   => content.screen("girl.thanks")
    case Step.Escort   => content.screen("girl.escort")
    case Step.CityThanks => content.screen("girl.cityThanks")
    case Step.Asked    => askedScreen(scene)
    case Step.CityMap  => content.screen("girl.cityMap")
    case Step.MapOffer => Screen(content.format("girl.mapOffer.text", "price" -> scene.price.toString), content.screen("girl.mapOffer").choices)
    case Step.CityTavern => content.screen("girl.cityTavern")
    case Step.Tavern   => content.screen("girl.tavern")
    case Step.Room     => content.screen("girl.room")
    case Step.Excuse   => content.screen("girl.excuse")
    case Step.Brother  => content.screen("girl.brother")
    case Step.Demand   => demandScreen(scene.price)
    case Step.PayConfirm => payConfirmScreen(scene.price)
    case Step.RoomWait => lightsOut
    case _             => Screen(content.text("girl.lightsOut"), Nil)
  }

  /** «Что ты там делала?» — девушка называет отвар, который собралась варить,
    * и одну траву из его состава: травы первого ранга и растут за городом.
    * Что именно, решаем один раз и кладём в сцену — на возврате к тому же шагу
    * она не должна говорить другое. */
  private def askWhatSheDid(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- requireScene(user)
      kinds  = BrewKind.rank1.toList
      kind  <- Random.nextIntBounded(kinds.size.max(1)).map(kinds.lift)
      herb  <- kind.fold(ZIO.succeed(Option.empty[MaterialKind]))(k =>
                 Random.nextIntBounded(k.recipe.size.max(1)).map(k.recipe.lift))
      next   = scene.copy(step = Step.Asked, brew = kind.map(_.entryName), herb = herb.map(_.entryName))
      _     <- writeScene(user, next)
      _     <- renderer.show(user, screenFor(next))
    } yield StateType.Girl

  /** Что она ответила: отвар и трава из сцены. Запись, начатая до этой правки,
    * их не несёт — тогда она говорит общими словами. */
  private def askedScreen(scene: GirlScene): Screen = {
    val said = for {
      kind <- scene.brew.flatMap(BrewKind.withNameOption)
      herb <- scene.herb.flatMap(MaterialKind.withNameOption)
    } yield content.format("girl.asked.text", "brew" -> kind.label, "herb" -> herb.displayName)
    Screen(said.getOrElse(content.text("girl.askedPlain")), content.screen("girl.asked").choices)
  }

  private def resume(user: User, renderer: Renderer): Task[StateType] =
    readScene(user).flatMap {
      case Some(scene) if scene.step == Step.RoomWait => roomWait(user, renderer, scene).as(StateType.Girl)
      case Some(scene) => renderer.show(user, screenFor(scene)).as(StateType.Girl)
      case None        => clear(user).as(StateType.Dungeon)
    }

  /** Репутация за выбор в городе. */
  private def reward(user: User, renderer: Renderer, rep: Hero => Long): Task[Unit] =
    for {
      hero <- getHero(user)
      gained = rep(hero)
      _    <- heroDao.updateGuildReputation(user.userId, hero.guildReputation + gained)
      _    <- renderer.show(user, Screen(content.format("girl.repGained", "rep" -> gained.toString), Nil))
    } yield ()

  /** Репутация с разбросом ±20%: уровень × `perLevel`. */
  private def rewardSpread(user: User, renderer: Renderer, perLevel: Long): Task[Unit] =
    Random.nextLongBetween(SpreadMin, SpreadMax + 1L).flatMap(spread =>
      reward(user, renderer, h => (h.lvl * perLevel * spread / 100L).max(1L)))

  private def leave(user: User, renderer: Renderer, key: String, to: StateType): Task[StateType] =
    renderer.show(user, Screen(content.text(key), Nil)) *> clear(user).as(to)

  private def clear(user: User): Task[Unit] = heroDao.writeSceneData(user.userId, Json.Null)

  private def readScene(user: User): Task[Option[GirlScene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[GirlScene].toOption))

  private def requireScene(user: User): Task[GirlScene] =
    readScene(user).flatMap(ZIO.fromOption(_)).orElseFail(new Throwable(s"No girl scene for user ${user.userId}"))

  private def writeScene(user: User, scene: GirlScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object GirlState {
  /** Ход события в `scene_data`. `price` — цена карты или требование брата,
    * `lost` — сколько серебра ушло в комнате, `roomStartedAt` — когда погас свет. */
  final case class GirlScene(
    step:          String,
    race:          String,
    price:         Long         = 0L,
    lost:          Long         = 0L,
    roomStartedAt: Option[Long] = None,
    /** Отвар, который она собралась варить, и трава из его состава — названные
      * один раз (см. GirlState.askWhatSheDid). */
    brew:          Option[String] = None,
    herb:          Option[String] = None
  )
  object GirlScene {
    implicit val encoder: Encoder[GirlScene] = (s: GirlScene) => Json.obj(
      "girlStep"      -> s.step.asJson,
      "race"          -> s.race.asJson,
      "price"         -> s.price.asJson,
      "lost"          -> s.lost.asJson,
      "roomStartedAt" -> s.roomStartedAt.asJson,
      "brew"          -> s.brew.asJson,
      "herb"          -> s.herb.asJson
    )
    implicit val decoder: Decoder[GirlScene] = (c: HCursor) =>
      for {
        step  <- c.get[String]("girlStep")
        race  <- c.get[String]("race")
        price <- c.getOrElse[Long]("price")(0L)
        lost  <- c.getOrElse[Long]("lost")(0L)
        start <- c.getOrElse[Option[Long]]("roomStartedAt")(None)
        brew  <- c.getOrElse[Option[String]]("brew")(None)
        herb  <- c.getOrElse[Option[String]]("herb")(None)
      } yield GirlScene(step, race, price, lost, start, brew, herb)
  }

  object Step {
    val Meet       = "meet"
    val Bandits    = "bandits"
    val AfterFight = "afterFight"
    val Thanks     = "thanks"
    val Escort     = "escort"
    val CityThanks = "cityThanks"
    val Asked      = "asked"
    val CityMap    = "cityMap"
    val MapOffer   = "mapOffer"
    val CityTavern = "cityTavern"
    val Tavern     = "tavern"
    val Room       = "room"
    val Excuse     = "excuse"
    val Brother    = "brother"
    val Demand     = "demand"
    /** Согласился отдать серебро — переспрашиваем, точно ли. */
    val PayConfirm = "payConfirm"
    val RoomWait   = "roomWait"
  }

  /** Трое бандитов, каждый — одного из первых трёх тиров. */
  val BanditCount: Int = 3
  val BanditTiers: IndexedSeq[Rarity] = IndexedSeq(Rarity.Common, Rarity.Uncommon, Rarity.Rare)

  /** Брат девицы — редкий человек уровня этажа. */
  val BrotherRace: Race     = Race.Human
  val BrotherRarity: Rarity = Rarity.Rare

  /** Благодарность после боя: серебро = уровень × 2 (±20%), репутация = уровень × 2. */
  val SilverPerLevel: Long    = 2L
  val RepPerLevelThanks: Long = 2L
  /** В городе: уровень × 3 за прощание; уровень × 10 (±20%) — за «попрощаться и
    * уйти» после расспросов и за пожелание удачи хозяйке карты. */
  val RepPerLevelCity: Long     = 3L
  val RepPerLevelFarewell: Long = 10L
  /** Уйти из таверны, не поднимаясь в комнату: уровень × 2. */
  val RepPerLevelTavern: Long = 2L
  /** Цена карты отца: уровень × 750 (±20%). */
  val MapPricePerLevel: Long  = 750L
  val SpreadMin: Long = 80L
  val SpreadMax: Long = 120L

  /** Исходы в городе, в процентах. */
  val CityThanksPct: Int = 48
  val CityMapPct: Int    = 2

  /** Уединение: кошелёк худеет на 25–50%, травмы пройдут через 90 минут, свет
    * гаснет на 30. */
  val LossPctMin: Long  = 25L
  val LossPctMax: Long  = 50L
  val HealDelayMs: Long = 90L * 60L * 1000L
  val RoomWaitMs: Long  = 30L * 60L * 1000L
  val LeaveRoomAction: String = """{"action":"LeaveRoom"}"""
}
