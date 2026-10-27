package pangea.service.state.states.race

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.race.RaceQuestState._
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Квест смены расы. Начинается с купона в сумке, только из города: выбор
  * расы, подтверждение — и сразу история этой расы, сцена за сценой, до конца.
  *
  * Боёв, травм и настоящей смерти нет — только текст и кнопки. Герой берётся
  * за обычное дело, а кончается оно тем, что его калечит и добивает кто-то
  * этой расы, и он просыпается в теле убийцы, как в прологе. Где у сцены два
  * выбора, оба ведут вперёд: меняется реплика в начале следующей сцены.
  *
  * В конце раса меняется, купон сгорает; здоровье и энергия срезаются под
  * потолок нового тела, травмы остаются. Ход квеста лежит в scene_data:
  * прервали (/home) — купон цел, квест можно начать снова. */
case class RaceQuestState(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "RacePick"   -> Target.Run { (u, ua, r) => pick(u, ua, r) },
      "RaceBack"   -> Target.Run { (u, _, r) => backToPick(u, r) },
      "RaceCancel" -> Target.Run { (u, _, r) => cancel(u, r) },
      "RaceGo"     -> Target.Run { (u, ua, r) => go(u, ua, r) },
      "RaceStep"   -> Target.Run { (u, ua, r) => step(u, ua, r) },
      "RaceHome"   -> Target.Run { (u, _, r) => home(u, r) }
    ),
    // Посторонний ввод и старые кнопки — тот же экран ещё раз.
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.RaceQuest) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Inventory, StateType.GlobalMap, StateType.RaceQuest)

  /** Экран, на котором остановились: выбор расы, подтверждение, сцена или финал. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      p    <- progress(user)
      _    <- renderer.show(user, screenOf(hero, p))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Выбор расы ──────────────────────────────────────────────────────────────

  /** Раса выбрана — подтверждение. Своя раса и чужие для героя (боссовые) не
    * выбираются; после начала истории выбор уже не меняется. */
  private def pick(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      p    <- progress(user)
      quest = payload(ua, "race").flatMap(Race.withNameOption).filter(r => Race.mortals.contains(r) && r != hero.race)
                .flatMap(RaceQuestState.of)
      _    <- quest match {
                case Some(q) if !p.started =>
                  save(user, Progress(Some(q.race.entryName), None, done = false)) *> renderer.show(user, confirmScreen(q))
                case _ => renderer.show(user, screenOf(hero, p))
              }
    } yield StateType.RaceQuest

  private def backToPick(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      p    <- progress(user)
      _    <- if (p.started) renderer.show(user, screenOf(hero, p))
              else save(user, Progress.start) *> renderer.show(user, pickScreen(hero))
    } yield StateType.RaceQuest

  /** «Не сейчас» — обратно в сумку. Начатую историю так не бросить: она идёт до конца. */
  private def cancel(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      p    <- progress(user)
      next <- if (p.started) renderer.show(user, screenOf(hero, p)).as(StateType.RaceQuest)
              else heroDao.writeSceneData(user.userId, Json.Null).as(StateType.Inventory)
    } yield next

  /** «Решено» — первая сцена истории. */
  private def go(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      p    <- progress(user)
      _    <- (p.quest, p.quest.flatMap(_.first)) match {
                case (Some(q), Some(first)) if !p.started && payload(ua, "race").contains(q.race.entryName) =>
                  save(user, p.copy(scene = Some(first.id))) *> renderer.show(user, sceneScreen(q, first, None))
                case _ => renderer.show(user, screenOf(hero, p))
              }
    } yield StateType.RaceQuest

  // ── История ─────────────────────────────────────────────────────────────────

  /** Выбор в сцене: к следующей — с репликой этого выбора, если она есть. Выбор
    * ведёт в финал — пробуждение в новом теле (см. [[awaken]]). */
  private def step(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      p    <- progress(user)
      next <- (p.quest, p.current) match {
                case (Some(q), Some(scene)) if !p.done =>
                  scene.picks.find(pk => payload(ua, "pick").contains(pk.id)).flatMap(pk => q.scene(pk.next).map(pk -> _)) match {
                    case Some((_, to)) if to.isFinal => awaken(user, hero, q, p, renderer)
                    case Some((pk, to)) =>
                      val lead = Option.when(pk.hasThen)(content.text(key(q, scene, pk.id + "Then")))
                      save(user, p.copy(scene = Some(to.id))) *>
                        renderer.show(user, sceneScreen(q, to, lead)).as(StateType.RaceQuest)
                    case None => renderer.show(user, screenOf(hero, p)).as(StateType.RaceQuest)
                  }
                case _ => renderer.show(user, screenOf(hero, p)).as(StateType.RaceQuest)
              }
    } yield next

  /** Пробуждение: раса — новая, купон — сгорает, здоровье и энергия — не выше
    * потолка нового тела. Купона в сумке нет — история обрывается, раса прежняя. */
  private def awaken(user: User, hero: Hero, q: RaceQuest, p: Progress, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      inv    <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      coupon  = inv.items.data.find(_.questItem.contains(QuestItemKind.RaceCoupon))
      next   <- coupon match {
                  case None =>
                    heroDao.writeSceneData(user.userId, Json.Null) *>
                      renderer.show(user, Screen(content.text("raceQuest.noCoupon"), Nil)).as(StateType.GlobalMap)
                  case Some(c) =>
                    val reborn = hero.copy(race = q.race)
                    val capped = hero.fightStats.copy(
                      hp     = hero.fightStats.hp.min(reborn.effectiveMaxHp(now)),
                      energy = hero.fightStats.energy.min(reborn.maxEnergy(now)))
                    for {
                      _ <- heroDao.updateRace(user.userId, q.race)
                      _ <- ZIO.when(capped != hero.fightStats)(heroDao.updateFightStats(user.userId, capped))
                      _ <- inventoryRepo.removeItem(c.id, hero.id).mapError(e => new Throwable(e.toString))
                      _ <- save(user, p.copy(done = true))
                      _ <- renderer.show(user, finalScreen(q))
                    } yield StateType.RaceQuest
                }
    } yield next

  /** «Вернуться в Кинэт» — после пробуждения. */
  private def home(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      p    <- progress(user)
      next <- if (p.done) heroDao.writeSceneData(user.userId, Json.Null).as(StateType.GlobalMap)
              else renderer.show(user, screenOf(hero, p)).as(StateType.RaceQuest)
    } yield next

  // ── Экраны ──────────────────────────────────────────────────────────────────

  private def screenOf(hero: Hero, p: Progress): Screen =
    p.quest match {
      case None                => pickScreen(hero)
      case Some(q) if p.done   => finalScreen(q)
      case Some(q) =>
        p.current match {
          case Some(scene) => sceneScreen(q, scene, None)
          case None        => confirmScreen(q)
        }
    }

  /** Все смертные расы, кроме своей, — по две в ряд, и «Не сейчас». */
  private def pickScreen(hero: Hero): Screen = {
    val races   = Race.mortals.toList.filterNot(_ == hero.race)
    val buttons = races.zipWithIndex.map { case (r, i) =>
      Choice("RacePick", r.toString, data = Map("race" -> r.entryName), row = Some(i / PerRow))
    }
    val rows = (races.size + PerRow - 1) / PerRow
    Screen(content.text("raceQuest.pick"), buttons :+ content.choice("RaceCancel", "raceQuest.cancelLabel").copy(row = Some(rows)))
  }

  /** Перед «Решено» — заметки Ашалдарона о расе, как при выборе расы в
    * прологе: присмотреться, нужна ли она. */
  private def confirmScreen(q: RaceQuest): Screen =
    Screen(q.race.description + "\n\n" + content.format("raceQuest.confirm", "title" -> title(q), "race" -> q.race.toString), List(
      content.choice("RaceGo", "raceQuest.goLabel").copy(data = Map("race" -> q.race.entryName), row = Some(0)),
      content.choice("RaceBack", "raceQuest.otherLabel").copy(row = Some(1)),
      content.choice("RaceCancel", "raceQuest.cancelLabel").copy(row = Some(1))))

  private def sceneScreen(q: RaceQuest, scene: Scene, lead: Option[String]): Screen =
    Screen(lead.fold("")(_ + "\n\n") + content.text(key(q, scene, "text")),
      scene.picks.zipWithIndex.map { case (pk, i) =>
        content.choice("RaceStep", key(q, scene, pk.id)).copy(data = Map("pick" -> pk.id), row = Some(i))
      })

  private def finalScreen(q: RaceQuest): Screen =
    Screen(q.ending.fold("")(e => content.text(key(q, e, "text")) + "\n\n") +
      content.format("raceQuest.done", "title" -> title(q), "race" -> q.race.toString),
      List(content.choice("RaceHome", "raceQuest.homeLabel")))

  private def title(q: RaceQuest): String = content.text(s"raceQuest.${q.key}.title")

  private def key(q: RaceQuest, scene: Scene, field: String): String = s"raceQuest.${q.key}.${scene.id}.$field"

  // ── Хранение ────────────────────────────────────────────────────────────────

  private def progress(user: User): Task[Progress] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Progress].toOption).getOrElse(Progress.start))

  private def save(user: User, p: Progress): Task[Unit] = heroDao.writeSceneData(user.userId, p.asJson)

  private def payload(ua: UserAction, field: String): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(field)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object RaceQuestState {

  /** Сколько рас в ряд на экране выбора. */
  val PerRow: Int = 2

  /** Выбор в сцене. Подпись — `raceQuest.<квест>.<сцена>.<id>`; `hasThen` — у
    * выбора есть реплика в начале следующей сцены: `<id>Then`. */
  final case class Pick(id: String, next: String, hasThen: Boolean = false)

  /** Сцена истории: текст — `raceQuest.<квест>.<id>.text`. Без выборов — финал:
    * пробуждение в новом теле. */
  final case class Scene(id: String, picks: List[Pick] = Nil) {
    def isFinal: Boolean = picks.isEmpty
  }

  /** История одной расы: ключ в scenes.yaml и сцены, первая — начало. */
  final case class RaceQuest(race: Race, key: String, scenes: List[Scene]) {
    def scene(id: String): Option[Scene] = scenes.find(_.id == id)
    def first: Option[Scene]             = scenes.headOption
    def ending: Option[Scene]            = scenes.find(_.isFinal)
  }

  /** Восемь историй — по одной на каждую смертную расу. */
  val quests: List[RaceQuest] = List(
    // «Рыжая»: девочка, кошка, склад контрабандистов — Кривой Ян.
    RaceQuest(Race.Human, "human", List(
      Scene("s1", List(Pick("take", "s2"), Pick("ignore", "s2", hasThen = true))),
      Scene("s2", List(Pick("ask", "s3", hasThen = true), Pick("go", "s3"))),
      Scene("s3", List(Pick("cat", "s4", hasThen = true), Pick("hide", "s4", hasThen = true))),
      Scene("s4", List(Pick("close", "s5"))),
      Scene("s5"))),
    // «Долг до утра»: кости у каравана — племянник хозяина.
    RaceQuest(Race.Khajiit, "khajiit", List(
      Scene("s1", List(Pick("small", "s2", hasThen = true), Pick("all", "s2", hasThen = true))),
      Scene("s2", List(Pick("tent", "s3"), Pick("fire", "s3", hasThen = true))),
      Scene("s3", List(Pick("breath", "s4"))),
      Scene("s4"))),
    // «Письмо с печатью»: письмо эльфам — договор исполнен.
    RaceQuest(Race.Elf, "elf", List(
      Scene("s1", List(Pick("take", "s2"), Pick("ask", "s2", hasThen = true))),
      Scene("s2", List(Pick("open", "s3", hasThen = true), Pick("keep", "s3"))),
      Scene("s3", List(Pick("run", "s4", hasThen = true), Pick("fight", "s4", hasThen = true))),
      Scene("s4", List(Pick("look", "s5"))),
      Scene("s5"))),
    // «Трёхклык»: охота с орком — раненого в горах не тащат.
    RaceQuest(Race.Orc, "orc", List(
      Scene("s1", List(Pick("agree", "s2"), Pick("haggle", "s2", hasThen = true))),
      Scene("s2", List(Pick("first", "s3", hasThen = true), Pick("follow", "s3", hasThen = true))),
      Scene("s3", List(Pick("teeth", "s4"))),
      Scene("s4"))),
    // «Замок без ключа»: замок на спор — краденые чертежи.
    RaceQuest(Race.Gnome, "gnome", List(
      Scene("s1", List(Pick("take", "s2"), Pick("look", "s2", hasThen = true))),
      Scene("s2", List(Pick("tell", "s3", hasThen = true), Pick("silent", "s3", hasThen = true))),
      Scene("s3", List(Pick("shield", "s4"))),
      Scene("s4"))),
    // «Кто ворует сети»: мурлочонок в сетях — первая добыча.
    RaceQuest(Race.Murloc, "murloc", List(
      Scene("s1", List(Pick("wait", "s2"), Pick("walk", "s2", hasThen = true))),
      Scene("s2", List(Pick("free", "s3", hasThen = true), Pick("tie", "s3", hasThen = true))),
      Scene("s3", List(Pick("water", "s4"))),
      Scene("s4"))),
    // «Настоящая карта»: гоблин-проводник и яма с кольями.
    RaceQuest(Race.Goblin, "goblin", List(
      Scene("s1", List(Pick("hire", "s2"), Pick("leave", "s2", hasThen = true))),
      Scene("s2", List(Pick("agree", "s3", hasThen = true), Pick("threaten", "s3", hasThen = true))),
      Scene("s3", List(Pick("laugh", "s4"))),
      Scene("s4"))),
    // «Скрежет в стене»: меловой круг в подвале таверны.
    RaceQuest(Race.Demon, "demon", List(
      Scene("s1", List(Pick("knock", "s2", hasThen = true), Pick("cellar", "s2"))),
      Scene("s2", List(Pick("erase", "s3", hasThen = true), Pick("leave", "s3", hasThen = true))),
      Scene("s3", List(Pick("silent", "s4"))),
      Scene("s4")))
  )

  def of(race: Race): Option[RaceQuest] = quests.find(_.race == race)

  /** Ход квеста в scene_data: раса выбрана (`race`), история начата
    * (`scene` — текущая сцена), пробуждение уже было (`done`). */
  final case class Progress(race: Option[String], scene: Option[String], done: Boolean) {
    def quest: Option[RaceQuest] = race.flatMap(Race.withNameOption).flatMap(RaceQuestState.of)
    def current: Option[Scene]   = for { q <- quest; id <- scene; s <- q.scene(id) } yield s
    def started: Boolean         = scene.isDefined || done
  }

  object Progress {
    val start: Progress = Progress(None, None, done = false)

    // Свой ключ, а не поля в корне: в scene_data лежат и чужие структуры.
    implicit val encoder: Encoder[Progress] = (p: Progress) =>
      Json.obj("raceQuest" -> Json.obj("race" -> p.race.asJson, "scene" -> p.scene.asJson, "done" -> p.done.asJson))

    implicit val decoder: Decoder[Progress] = (c: HCursor) =>
      c.downField("raceQuest").as[Json].flatMap { node =>
        val q = node.hcursor
        for {
          race  <- q.getOrElse[Option[String]]("race")(None)
          scene <- q.getOrElse[Option[String]]("scene")(None)
          done  <- q.getOrElse[Boolean]("done")(false)
        } yield Progress(race, scene, done)
      }
  }
}
