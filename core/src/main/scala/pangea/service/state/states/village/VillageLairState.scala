package pangea.service.state.states.village

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject, jawn}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.cave.CaveRates
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails}
import pangea.model.monster.{Monster, Race}
import pangea.model.quest.Difficulty
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.model.village.{BanditAftermath, NestPrep, VillageContract, VillageQuest, VillageRates}
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.events.cave.{CaveSupply, NestBoon, Supply}
import pangea.service.state.{CharacterMenu, ItemMenu, NpcQuestLog, State, UserAction, VillageLair, Villages}
import zio.{Random, Task, ZIO}

/** Логово беды деревни — куда ведёт карточка «Контракта деревни», из
  * лабиринта (см. [[Villages]]). У каждой беды своё логово ([[VillageLair]]):
  * банда на заставе, гнездо сетеплётов в овраге; сцена одна, отличаются
  * тексты (`village.<логово>.*`), кнопки (`<приставка>Fight`…) и бой.
  *
  * У входа видно, кто там и насколько опасен. «Напасть» — один сюжетный бой со
  * всеми обитателями логова уровня задания: без «Сбежать», награда как за
  * обычный бой, отряд с героем. Добыча возвращает сюда (отметка `lair.wonKey`):
  * логово разбито, дальше — к старосте за платой.
  *
  * У гнезда сетеплётов, как на пороге пещеры, можно пустить в дело вещь из
  * сумки или с пояса (см. [[NestPrep]], `CaveSupply.nestBoonOf`): дым слабит
  * пауков, огонь поджигает их ещё до боя, паучий цвет уводит выводок.
  *
  * «Уйти», смерть или ничья контракт не двигают: он при герое, логово ждёт.
  * «Отказаться от контракта» — с подтверждением: контракт и договор уходят, и
  * деревни снова встречаются.
  *
  * На заставе разбойников после боя голову главаря забирают с собой — староста
  * за неё доплачивает ([[VillageRates.HeadDoubloons]] дублонов). С шансом
  * [[VillageRates.HuntersPct]]% на выходе ждут охотники за головами: отдать им
  * голову или драться — сюжетный бой уровня задания. Пал в нём — голову
  * забрали, а логово всё равно разбито. Что ещё ждёт на заставе, лежит в
  * scene_data ([[BanditAftermath]]); контракт к этому времени уже «сделано». */
case class VillageLairState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  lair:          VillageLair
) extends State {

  private val ids = lair.ids

  /** Пускают ли здесь вещь в дело у входа: пока только у гнезда сетеплётов. */
  private val stocked: Boolean = lair.quest == VillageQuest.SpiderNests

  /** Ждёт ли что-то после боя: голова главаря и всё, что с ней связано, — на
    * заставе разбойников. */
  private val hunted: Boolean = lair.quest == VillageQuest.BanditLair

  private val usePrefix  = s"${ids}Use_"
  private val beltPrefix = s"${ids}Belt_"

  private val branch = new Branch(
    routes = Map(
      s"${ids}Fight"      -> Target.Run { (u, _, r) => atLair(u, r)(c => fight(u, c, r)) },
      s"${ids}Supply"     -> Target.Run { (u, _, r) => atLair(u, r)(_ => supplies(u, r)) },
      s"${ids}SupplyPrev" -> Target.Run { (u, _, r) => atLair(u, r)(_ => turnPage(u, r, -1)) },
      s"${ids}SupplyNext" -> Target.Run { (u, _, r) => atLair(u, r)(_ => turnPage(u, r, +1)) },
      s"${ids}Leave"      -> Target.Run { (u, _, r) => leave(u, r) },
      s"${ids}Abandon"    -> Target.Run { (u, _, r) => atLair(u, r)(c => askAbandon(u, c, r)) },
      s"${ids}AbandonYes" -> Target.Run { (u, _, r) => atLair(u, r)(_ => abandon(u, r)) },
      s"${ids}Back"       -> Target.Run { (u, _, r) => enter(u, r).as(lair.scene) },
      s"${ids}Out"        -> Target.Run { (u, _, r) => out(u, r) },
      s"${ids}HuntersGive"  -> Target.Run { (u, _, r) => afterFight(u, r)((c, a) => giveHead(u, c, a, r)) },
      s"${ids}HuntersFight" -> Target.Run { (u, _, r) => afterFight(u, r)((c, a) => huntersFight(u, c, a, r)) },
      s"${ids}SurvivorsKill"  -> Target.Run { (u, _, r) => afterFight(u, r)((c, a) => killSurvivors(u, c, a, r)) },
      s"${ids}SurvivorsSpare" -> Target.Run { (u, _, r) => afterFight(u, r)((c, a) => spareSurvivors(u, c, a, r)) },
      "OpenCharacter"     -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, lair.scene) }
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Dungeon, StateType.Battle, StateType.HeroStats, StateType.Loot, lair.scene)

  /** Вход: обитатели перебиты (добыча положила отметку) — логово разбито;
    * логово ещё стоит — вход в него. На заставе разбойников после боя: охотники
    * перебиты — голова ваша; ещё что-то ждёт — оно. Иначе здесь пусто. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      contract <- standing(user)
      data     <- heroDao.readSceneData(user.userId)
      won       = data.exists(VillageLairState.won(lair, _))
      cleared  <- clearedContract(user)
      after     = data.flatMap(_.as[BanditAftermath].toOption)
      _        <- (contract, cleared) match {
                    case (Some(c), _) if won                                   => victory(user, c, renderer)
                    case (Some(c), _)                                          => renderer.show(user, introScreen(c))
                    case (None, Some(c)) if data.exists(VillageLairState.huntersBeaten) => afterHunters(user, c, renderer)
                    case (None, Some(c)) => after match {
                      case Some(a) if !a.done => renderer.show(user, stepScreen(c, a))
                      // Вернулись с добычи (тайник сдавшихся), а ждать больше нечего.
                      case Some(_) => clear(user) *> renderer.show(user,
                                        finalScreen(content.format(key("leftOutpost"), Villages.args(content, c): _*)))
                      case None    => clear(user) *> renderer.show(user, Screen(text("gone"), List(outChoice)))
                    }
                    case _ =>
                      clear(user) *> renderer.show(user, Screen(text("gone"), List(outChoice)))
                  }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Контракт на это логово, пока оно стоит. */
  private def standing(user: User): Task[Option[VillageContract]] =
    NpcQuestLog.load(heroDao, user.userId).map(q => q.village.filter(_ => Villages.toLair(q, lair.quest)))

  /** Кнопка логова — пока логово стоит; иначе показать, что сейчас. */
  private def atLair(user: User, renderer: Renderer)(f: VillageContract => Task[StateType]): Task[StateType] =
    standing(user).flatMap {
      case Some(c) => f(c)
      case None    => enter(user, renderer).as(lair.scene)
    }

  /** Контракт на заставу, которую уже разбили: после боя на ней ещё что-то
    * ждёт. У прочих логов — ничего. */
  private def clearedContract(user: User): Task[Option[VillageContract]] =
    if (!hunted) ZIO.none
    else NpcQuestLog.load(heroDao, user.userId).map(_.village.filter(c => c.quest == lair.quest && c.done))

  private def readAftermath(user: User): Task[Option[BanditAftermath]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[BanditAftermath].toOption).filterNot(_.done))

  /** Кнопка «после боя» — пока на заставе ещё что-то ждёт; иначе показать, что
    * сейчас. */
  private def afterFight(user: User, renderer: Renderer)(f: (VillageContract, BanditAftermath) => Task[StateType]): Task[StateType] =
    for {
      cleared <- clearedContract(user)
      after   <- readAftermath(user)
      res     <- (cleared, after) match {
                   case (Some(c), Some(a)) => f(c, a)
                   case _                  => enter(user, renderer).as(lair.scene)
                 }
    } yield res

  // ── Тексты и экраны ─────────────────────────────────────────────────────────

  /** Текст этого логова: `village.<логово>.<key>`. */
  private def key(k: String): String = s"village.${lair.texts}.$k"

  private def text(k: String): String = content.text(key(k))

  /** Вход в логово: что там, сложность и что из этого боя не сбежать. У гнезда
    * — ещё и «Использовать предмет». */
  private def introScreen(c: VillageContract): Screen =
    Screen(
      Villages.joined(content.format(key("intro"), Villages.args(content, c): _*), difficulty(c.level),
        content.text("village.lair.noEscape")),
      List(
        Some(content.choice(s"${ids}Fight", key("fightLabel")).copy(row = Some(0))),
        Option.when(stocked)(content.choice(s"${ids}Supply", key("supplyLabel")).copy(row = Some(0))),
        Some(content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(1))),
        Some(content.choice(s"${ids}Leave", "village.lair.leaveLabel").copy(row = Some(1))),
        Some(content.choice(s"${ids}Abandon", "village.lair.abandonLabel").copy(row = Some(2)))).flatten)

  private def difficulty(level: Int): String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(level.max(1)))

  private def outChoice = content.choice(s"${ids}Out", "village.toDungeonLabel")

  // ── Вещь, пущенная в дело у входа ───────────────────────────────────────────

  /** Скарб героя кнопками, как на пороге пещеры: сперва пояс, потом сумка.
    * Стопку в дело не пускают — вещи из неё идут в дело только распакованными. */
  private def supplies(user: User, renderer: Renderer, note: String = ""): Task[StateType] =
    if (!stocked) enter(user, renderer).as(lair.scene)
    else
      for {
        hero <- getHero(user)
        inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
        prep <- readPrep(user)
        all   = CaveSupply.beltSupplies(hero.equipment.belt) ++ inv.items.data.filterNot(_.isBundle).map(Supply.Carried)
        _    <- if (all.isEmpty) renderer.show(user, Screen(text("supply.empty"), List(backChoice)).noted(note))
                else {
                  val (pageItems, pages, p) = ItemMenu.page(all, prep.page)
                  val buttons = pageItems.zipWithIndex.map {
                    case (Supply.OnBelt(slot, item), i) =>
                      Choice(s"$beltPrefix$slot", ItemMenu.truncate(content.format("common.onBelt", "name" -> item.displayTitle)),
                        row = Some(i))
                    case (Supply.Carried(item), i) =>
                      Choice(s"$usePrefix${item.id}", ItemMenu.itemButtonLabel(item), row = Some(i))
                  }
                  val nav = List(
                    Some(backChoice),
                    Option.when(p > 0)(Choice(s"${ids}SupplyPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
                    Option.when(p < pages - 1)(Choice(s"${ids}SupplyNext", content.text("common.next"), row = Some(ItemMenu.NavRow)))
                  ).flatten
                  renderer.show(user, Screen(content.format(key("supply.pick"),
                    "page" -> (p + 1).toString, "total" -> pages.toString), buttons ++ nav).noted(note))
                }
      } yield lair.scene

  private def backChoice: Choice =
    content.choice(s"${ids}Back", key("supply.back")).copy(color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))

  private def turnPage(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    readPrep(user).flatMap(p => writePrep(user, p.copy(page = (p.page + delta).max(0)))) *> supplies(user, renderer)

  /** Вещь из сумки: у заряженной уходит один заряд, прочее — целиком. Сюжетное
    * не трогаем: его не теряют нигде. */
  private def useItem(user: User, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res  <- inv.items.data.find(i => i.id == itemId && !i.isBundle) match {
                case None => supplies(user, renderer)
                case Some(item) if item.isQuestItem || item.isStoryGear =>
                  supplies(user, renderer, content.format(key("supply.quest"), "name" -> item.displayTitle))
                case Some(item) => spend(user, item, renderer)(takeFrom(hero, item, CaveSupply.charged(item)))
              }
    } yield res

  /** Вещь из ячейки пояса — как из сумки: ячейка помнит, чем была полна. */
  private def useBelt(user: User, slot: Int, renderer: Renderer): Task[StateType] =
    getHero(user).flatMap { hero =>
      CaveSupply.beltSupplies(hero.equipment.belt).find(_.slot == slot) match {
        case Some(s) =>
          spend(user, s.item, renderer)(heroDao.updateEquipment(user.userId,
            hero.equipment.copy(belt = CaveSupply.beltTaken(hero.equipment.belt, slot))))
        case None => supplies(user, renderer)
      }
    }

  /** Пустить вещь в дело: она уходит (`take`), а гнездо получает своё (см.
    * `CaveSupply.nestBoonOf`). Второй раз то же самое ничего не добавляет — как
    * и в пещере, вещь при этом всё равно уходит, о чём говорим честно. */
  private def spend(user: User, item: Item, renderer: Renderer)(take: Task[Unit]): Task[StateType] =
    if (CaveSupply.charged(item).exists(_.charges <= 0))
      supplies(user, renderer, content.format(key("supply.emptyFlask"), "name" -> item.displayTitle))
    else
      readPrep(user).flatMap { prep =>
        val boon = CaveSupply.nestBoonOf(item)
        val already = boon match {
          case NestBoon.Dope   => prep.weakened
          case NestBoon.Poison => prep.poisoned
          case NestBoon.Burn   => prep.burning
          case NestBoon.Lure   => prep.lured
          case NestBoon.None   => false
        }
        val next = boon match {
          case NestBoon.Dope   => prep.copy(weakened = true)
          case NestBoon.Poison => prep.copy(poisoned = true)
          case NestBoon.Burn   => prep.copy(burning = true)
          case NestBoon.Lure   => prep.copy(lured = true)
          case NestBoon.None   => prep
        }
        val name = "name" -> item.displayTitle
        val line = boon match {
          case _ if already    => content.format(key("supply.already"), name)
          case NestBoon.Dope   => content.format(key("supply.dope"), name, "pct" -> CaveRates.DopeCutPct.toString)
          case NestBoon.Poison => content.format(key("supply.poison"), name)
          case NestBoon.Burn   => content.format(key("supply.burn"), name, "pct" -> VillageRates.NestBurnPct.toString)
          // «Двое красных» — словом в самом тексте (см. VillageRates.LuredRedSpiders).
          case NestBoon.Lure   => content.format(key("supply.lure"), name)
          case NestBoon.None   => content.format(key("supply.wasted"), name)
        }
        take *> writePrep(user, next) *> supplies(user, renderer, line)
      }

  private def takeFrom(hero: Hero, item: Item, charged: Option[ItemDetails.Charged]): Task[Unit] =
    charged match {
      case Some(c) => inventoryRepo.updateItem(hero.id, item.copy(details = c.spent)).mapError(e => new Throwable(e.toString)).unit
      case None    => inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)).unit
    }

  // ── Бой ─────────────────────────────────────────────────────────────────────

  /** Бой со всеми обитателями логова. Добыча вернёт сюда — с отметкой, что они
    * перебиты. У гнезда подготовка входа ложится на бой: паучий цвет оставил
    * матку и красных, дым слабит, гриб травит, огонь жжёт — всех, кто вышел. */
  private def fight(user: User, contract: VillageContract, renderer: Renderer): Task[StateType] =
    for {
      hero     <- getHero(user)
      seed     <- Random.nextLong
      prep     <- readPrep(user)
      lured     = stocked && prep.lured
      foes0     = if (lured) Villages.luredBrood(contract) else Villages.lairMonsters(contract, Rng(seed))
      foes      = if (prep.weakened) foes0.map(CaveSupply.weaken) else foes0
      energies <- ZIO.foreach(foes)(startEnergy).map(es => if (prep.weakened) es.map(CaveSupply.cutDope) else es)
      plain     = Villages.withBreedTraits(SoloPveBattle.fromGroup(foes, hero, energies)
                    .copy(story = Some(lair.story), lootLevel = Some(contract.level.max(1).toLong)))
      poisoned  = if (prep.poisoned) CaveSupply.poisonAll(plain) else plain
      battle    = if (prep.burning) CaveSupply.burnAll(poisoned, VillageRates.NestBurnPct) else poisoned
      // Выполз красный сетеплёт — сказать об этом сразу, в начале боя. Цвет
      // увёл выводок — у боя своё начало: внизу только матка и красные.
      red       = foes.exists(_.breed.contains(pangea.model.monster.Breed.RedSpider))
      routing   = LootData(Nil, Nil, returnState = Some(lair.scene), eventData = Some(VillageLairState.wonMark(lair)))
      _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _        <- heroDao.writeSceneData(user.userId, routing.asJson)
      _        <- renderer.show(user, Screen(
                    if (lured) content.format(key("fightLured"), Villages.args(content, contract): _*)
                    else Villages.joined(
                      content.format(key("fight"), Villages.args(content, contract): _*),
                      if (red) content.text(key("redSpider")) else ""), Nil))
    } yield StateType.Battle

  /** Обитатели перебиты: логово разбито — к старосте за платой. На заставе
    * разбойников голову главаря забирают с собой, а на выходе с шансом ждут
    * охотники за головами. */
  private def victory(user: User, contract: VillageContract, renderer: Renderer): Task[Unit] = {
    val found = content.format(key("victory"), Villages.args(content, contract): _*)
    for {
      _ <- clear(user)
      _ <- Villages.lairCleared(heroDao, user.userId, lair.quest)
      _ <- if (!hunted) renderer.show(user, finalScreen(found))
           else for {
             hero      <- getHero(user)
             _         <- Villages.giveHead(inventoryRepo, itemRepo, hero)
             survivors <- rollSurvivors
             roll      <- Random.nextIntBetween(0, 100)
             _         <- next(user, contract, BanditAftermath(hunters = roll < VillageRates.HuntersPct, survivors = survivors),
                            renderer, found)
           } yield ()
    } yield ()
  }

  /** Сдадутся ли после боя и кто: столько разных смертных рас — или никого. */
  private def rollSurvivors: Task[List[Race]] =
    Random.nextIntBetween(0, 100).flatMap { roll =>
      if (roll >= VillageRates.SurvivorsPct) ZIO.succeed(List.empty[Race])
      else ZIO.foldLeft(1 to VillageRates.SurvivorsCount)((List.empty[Race], Race.mortals.toList)) {
        case ((picked, left), _) =>
          Random.nextIntBounded(left.size.max(1)).map(i =>
            left.lift(i).fold((picked, left))(r => (picked :+ r, left.filterNot(_ == r))))
      }.map(_._1)
    }

  /** Расы словами, через запятую и «и»: «человек, эльф и мурлок». */
  private def raceList(races: List[Race]): String = {
    val names = races.map(_.toString.toLowerCase)
    names.lastOption.fold("") { last =>
      val rest = names.dropRight(1)
      if (rest.isEmpty) last else rest.mkString(", ") + " и " + last
    }
  }

  // ── После боя на заставе ────────────────────────────────────────────────────

  /** Что дальше на заставе: ждёт ещё что-то — оно, с `note` над ним; нет —
    * к старосте. */
  private def next(user: User, c: VillageContract, after: BanditAftermath, renderer: Renderer, note: String): Task[Unit] =
    if (after.done) clear(user) *> renderer.show(user, finalScreen(note))
    else heroDao.writeSceneData(user.userId, after.asJson) *> renderer.show(user, stepScreen(c, after).noted(note))

  /** Логово разбито: что было — и как вернуться за платой. */
  private def finalScreen(note: String): Screen =
    Screen(Villages.joined(note, content.text("village.lair.returnHint")), List(outChoice))

  /** Что ждёт на заставе сейчас: сдавшиеся — перерезать или отпустить; на
    * выходе — охотники за головами: отдать голову или драться. */
  private def stepScreen(c: VillageContract, a: BanditAftermath): Screen =
    if (a.survivors.nonEmpty)
      Screen(
        content.format(key("survivors.intro"), Villages.args(content, c) :+ ("survivors" -> raceList(a.survivors)): _*),
        List(
          content.choice(s"${ids}SurvivorsKill", key("survivors.killLabel")).copy(row = Some(0)),
          content.choice(s"${ids}SurvivorsSpare", key("survivors.spareLabel")).copy(row = Some(0)),
          content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(1))))
    else
      Screen(
        Villages.joined(content.format(key("hunters.intro"), Villages.args(content, c): _*), content.text("village.lair.noEscape")),
        List(
          content.choice(s"${ids}HuntersGive", key("hunters.giveLabel")).copy(row = Some(0)),
          content.choice(s"${ids}HuntersFight", key("hunters.fightLabel")).copy(row = Some(0)),
          content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(1))))

  /** Перерезали сдавшихся: в кошелях у каждого — немного серебра
    * ([[VillageRates.SurvivorPursePerLvl]] за уровень задания), оно — герою. */
  private def killSurvivors(user: User, c: VillageContract, a: BanditAftermath, renderer: Renderer): Task[StateType] =
    if (a.survivors.isEmpty) enter(user, renderer).as(lair.scene)
    else {
      val purse = VillageRates.SurvivorPursePerLvl * c.level.max(1).toLong * a.survivors.size.toLong
      for {
        hero <- getHero(user)
        _    <- heroDao.updateSilver(user.userId, hero.silver + purse)
        _    <- next(user, c, a.copy(survivors = Nil), renderer, Villages.joined(
                  content.format(key("survivors.killed"), Villages.args(content, c): _*),
                  content.format(key("survivors.purse"), "silver" -> purse.toString)))
      } yield lair.scene
    }

  /** Отпустили сдавшихся: один из них показывает тайник — награбленное с обозов
    * ([[VillageRates.CacheMin]]…[[VillageRates.CacheMax]] вещей). Вещи — через
    * экран добычи: он и вернёт сюда, к тому, что ещё ждёт на заставе. */
  private def spareSurvivors(user: User, c: VillageContract, a: BanditAftermath, renderer: Renderer): Task[StateType] =
    a.survivors.headOption match {
      case None => enter(user, renderer).as(lair.scene)
      case Some(informer) =>
        for {
          n    <- Random.nextIntBetween(VillageRates.CacheMin, VillageRates.CacheMax + 1)
          seed <- Random.nextLong
          (cache, _) = pangea.generator.loot.LootGenerator.banditCache(n, Rng(seed))
          rest  = a.copy(survivors = Nil)
          _    <- heroDao.writeSceneData(user.userId, LootData(cache, Nil, returnState = Some(lair.scene),
                    eventData = Some(rest.asJson)).asJson)
          _    <- renderer.show(user, Screen(content.format(key("survivors.spared"),
                    Villages.args(content, c) :+ ("informer" -> informer.toString): _*), Nil))
        } yield StateType.Loot
    }

  /** Отдал голову охотникам: они уходят, доплаты за голову не будет, а за
    * логово староста всё равно заплатит. */
  private def giveHead(user: User, c: VillageContract, a: BanditAftermath, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      _    <- Villages.dropHead(inventoryRepo, hero)
      _    <- next(user, c, a.copy(hunters = false), renderer, content.format(key("hunters.given"), Villages.args(content, c): _*))
    } yield lair.scene

  /** Драться с охотниками: сюжетный бой уровня задания. Добыча вернёт сюда с
    * отметкой, что охотники перебиты. */
  private def huntersFight(user: User, c: VillageContract, a: BanditAftermath, renderer: Renderer): Task[StateType] =
    for {
      hero     <- getHero(user)
      seed     <- Random.nextLong
      foes      = Villages.bountyHunters(c, Rng(seed))
      energies <- ZIO.foreach(foes)(startEnergy)
      battle    = SoloPveBattle.fromGroup(foes, hero, energies)
                    .copy(story = Some(Villages.HuntersStory), lootLevel = Some(c.level.max(1).toLong))
      routing   = LootData(Nil, Nil, returnState = Some(lair.scene),
                    eventData = Some(a.copy(hunters = false).asJson.deepMerge(VillageLairState.huntersBeatenMark)))
      _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _        <- heroDao.writeSceneData(user.userId, routing.asJson)
      _        <- renderer.show(user, Screen(content.format(key("hunters.fight"), Villages.args(content, c): _*), Nil))
    } yield StateType.Battle

  /** Охотники перебиты: голова ваша — дальше что ещё ждёт на заставе или к
    * старосте. */
  private def afterHunters(user: User, c: VillageContract, renderer: Renderer): Task[Unit] =
    for {
      data <- heroDao.readSceneData(user.userId)
      after = data.flatMap(_.as[BanditAftermath].toOption).getOrElse(BanditAftermath())
      _    <- next(user, c, after, renderer, content.format(key("hunters.victory"), Villages.args(content, c): _*))
    } yield ()

  // ── Уход и отказ ────────────────────────────────────────────────────────────

  /** «Уйти»: логово ждёт, контракт при герое. Подготовка входа пропадает: дым
    * развеется, огонь прогорит, выводок вернётся. На заставе после боя это
    * старая кнопка: показать, что там ждёт. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    readAftermath(user).flatMap {
      case Some(_) => enter(user, renderer).as(lair.scene)
      case None    => clear(user) *> renderer.show(user, Screen(text("left"), Nil)).as(StateType.Dungeon)
    }

  /** «В лабиринт»: на заставе после боя ещё что-то ждёт — старая кнопка. */
  private def out(user: User, renderer: Renderer): Task[StateType] =
    readAftermath(user).flatMap {
      case Some(_) => enter(user, renderer).as(lair.scene)
      case None    => clear(user).as(StateType.Dungeon)
    }

  private def askAbandon(user: User, contract: VillageContract, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.format("village.lair.abandon.text", Villages.args(content, contract): _*), List(
      content.choice(s"${ids}AbandonYes", "village.lair.abandon.yesLabel").copy(row = Some(0)),
      content.choice(s"${ids}Back", "village.lair.abandon.noLabel").copy(row = Some(0))))).as(lair.scene)

  /** Отказался: контракт и договор уходят — деревни снова встречаются. */
  private def abandon(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      _    <- Villages.abandon(heroDao, inventoryRepo, user.userId, hero)
      _    <- clear(user)
      _    <- renderer.show(user, Screen(content.text("village.lair.abandon.done"), Nil))
    } yield StateType.Dungeon

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  /** Кнопки вещей (`<приставка>Use_<id>`, `<приставка>Belt_<ячейка>`) — у
    * гнезда; прочее и старые кнопки — вход, как было. */
  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] = {
    val action = ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))
    action match {
      case Some(a) if stocked && a.startsWith(usePrefix) =>
        a.drop(usePrefix.length).toLongOption.fold(supplies(user, renderer)) { id =>
          atLair(user, renderer)(_ => useItem(user, id, renderer))
        }
      case Some(a) if stocked && a.startsWith(beltPrefix) =>
        a.drop(beltPrefix.length).toIntOption.fold(supplies(user, renderer)) { slot =>
          atLair(user, renderer)(_ => useBelt(user, slot, renderer))
        }
      case _ => enter(user, renderer).as(lair.scene)
    }
  }

  private def readPrep(user: User): Task[NestPrep] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[NestPrep].toOption).getOrElse(NestPrep.empty))

  private def writePrep(user: User, prep: NestPrep): Task[Unit] =
    heroDao.writeSceneData(user.userId, prep.asJson)

  private def startEnergy(m: Monster): Task[Long] =
    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct))

  private def clear(user: User): Task[Unit] = heroDao.writeSceneData(user.userId, Json.Null)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object VillageLairState {

  /** Отметка в scene_data, что обитатели логова перебиты: её пишет добыча,
    * читает вход. Ключ у каждого логова свой (`lair.wonKey`): в scene_data
    * лежат и чужие структуры, и чужое поле не должно читаться как наше. */
  def wonMark(lair: VillageLair): Json = Json.fromJsonObject(JsonObject(lair.wonKey -> true.asJson))

  def won(lair: VillageLair, sceneData: Json): Boolean =
    sceneData.hcursor.get[Boolean](lair.wonKey).toOption.contains(true)

  /** Отметка в scene_data, что охотники за головами на выходе с заставы
    * перебиты: её пишет добыча, читает вход. */
  val HuntersBeatenKey: String = "villageHuntersBeaten"

  val huntersBeatenMark: Json = Json.fromJsonObject(JsonObject(HuntersBeatenKey -> true.asJson))

  def huntersBeaten(sceneData: Json): Boolean = sceneData.hcursor.get[Boolean](HuntersBeatenKey).toOption.contains(true)
}
