package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.{Choice, Renderer, SceneContent, Screen}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{BattleAlly, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.{AllyKind, UndeadForm}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.StoryLine.Survivors
import pangea.service.state.StoryToolQuest.Path
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Глава 6 основной линии, «Наутро» ([[NpcQuest.StoryMorning]], см.
  * [[StoryLine]]) — первая глава Акта II. Что осталось после кладбища: у
  * каждого пути Рубежа I ([[Path]]) своя история, а кончаются все одним —
  * описью урожая: Сборщик приедет к первому снегу, и новое имя героя в описи
  * третьим.
  *
  * Начинается дома, наутро (см. [[atHome]]): у Тени под запертой дверью
  * записка от Э. (её текст — от того, кто пережил пролог), у Щита — посыльный
  * от Юноны, у одиночки — книга ячейки, читанная ночью. Ведёт главу у каждого
  * пути свой предмет ([[lead]]): поручение Э., грамота совета, книга ячейки.
  *
  * Тень — 1: Горн (где прячется его писарь, которого Братство велело вывезти);
  * 2: «Три кости» — откупиться или драться с вышибалами (StoryDenState).
  * Щит — 1: Юнона просит стеречь Брея; 2: Горн даёт людей и идёт сам; 3: ночь
  * в подвале совета, чистильщики (StoryCellarState), Горн и двое искателей —
  * в строю; 4: Юнона — совет оставил Брею жизнь за показания. Сам по себе — 1:
  * Барти находит себя в книге ячейки и становится партнёром; 2: люди Братства
  * приходят за книгой — Барти со сковородой в строю (BartiHouseState).
  * Враги, сложность и награда — по уровню задания, а не героя. */
object StoryMorningQuest {

  /** Уровень задания: по нему сложность, враги, гости в строю и награда. */
  val Level: Int = 26

  /** Ключи сюжетных боёв главы: награда как за обычный бой, без «Сбежать». */
  val DenStory: String    = "storyDen"
  val CellarStory: String = "storyCellar"
  val BartiStory: String  = "storyBartiHouse"

  /** Все бои главы — разом: для подсчёта и для смерти. */
  val FightStories: Set[String] = Set(DenStory, CellarStory, BartiStory)

  /** Тень: хозяин «Трёх костей» — именной человек, вышибалы — люди и орки. */
  val OwnerRarity: Rarity      = Rarity.Mythical
  val BouncerRarity: Rarity    = Rarity.Rare
  val BouncerRaces: List[Race] = List(Race.Human, Race.Orc, Race.Human, Race.Orc)

  /** Щит и одиночка: именной главарь людей Братства и четверо его людей. */
  val FoeRace: Race           = Race.Human
  val LeaderRarity: Rarity    = Rarity.Mythical
  val HenchmanRarity: Rarity  = Rarity.Rare
  val Henchmen: Int           = 4

  /** Гости в строю: Горн — легендарный, его искатели — редкие, Барти —
    * необычный: дерётся он плохо. */
  val HornRarity: Rarity   = Rarity.Legendary
  val SeekerRarity: Rarity = Rarity.Rare
  val BartiRarity: Rarity  = Rarity.Uncommon
  val GuestRace: Race      = Race.Human

  /** Серебро за главу — за уровень задания, как в Акте I. */
  val SilverPerLvl: Long = 200L

  /** Долг писаря в «Трёх костях» — за уровень задания: 26 000 серебра на
    * 26-м (так решил пользователь), откуп дороже награды за главу. */
  val DebtPerLvl: Long = 1000L

  def silver: Long = SilverPerLvl * Level
  def debt: Long   = DebtPerLvl * Level

  /** Путь Рубежа I — по нему ветка главы. */
  def path(q: NpcQuests): Option[Path] = StoryToolQuest.choice(q)

  private def on(q: NpcQuests, p: Path, n: Int): Boolean = path(q).contains(p) && q.onStep(NpcQuest.StoryMorning, n)

  // Тень.
  def toHorn(q: NpcQuests): Boolean = on(q, Path.Shadow, 1)
  def toDen(q: NpcQuests): Boolean  = on(q, Path.Shadow, 2)

  // Щит.
  def toJunona(q: NpcQuests): Boolean    = on(q, Path.Shield, 1)
  def toHornMen(q: NpcQuests): Boolean   = on(q, Path.Shield, 2)
  def toCellar(q: NpcQuests): Boolean    = on(q, Path.Shield, 3)
  def toTestimony(q: NpcQuests): Boolean = on(q, Path.Shield, 4)

  // Сам по себе.
  def toBarti(q: NpcQuests): Boolean      = on(q, Path.Alone, 1)
  def toBartiFight(q: NpcQuests): Boolean = on(q, Path.Alone, 2)

  /** К Барти с карточки книги: поговорить или встретить тех, кто пришёл за книгой. */
  def atBarti(q: NpcQuests): Boolean = toBarti(q) || toBartiFight(q)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryMorning)(_.copy(step = n))

  /** Главу пора начать: Рубеж I пройден, путь записан, глава ещё не взята. */
  def canStart(q: NpcQuests): Boolean = path(q).isDefined && StoryLine.canBegin(q, NpcQuest.StoryMorning)

  /** Что ведёт главу у пути: поручение Э. у Тени, грамота совета у Щита, книга
    * ячейки у одиночки. */
  def lead(p: Path): QuestItemKind = p match {
    case Path.Shadow => QuestItemKind.StoryShadowOrder
    case Path.Shield => QuestItemKind.StoryCouncilWrit
    case Path.Alone  => QuestItemKind.StoryCellBook
  }

  /** Строка карточки ведущего предмета — что делать сейчас. Глава не взята,
    * уже закрыта или предмет не этого пути — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests, kind: QuestItemKind): String =
    path(q).filter(p => lead(p) == kind && q.isTaken(NpcQuest.StoryMorning))
      .flatMap(p => content.list(s"story.storyMorning.card.${p.key}").lift(q.of(NpcQuest.StoryMorning).step - 1))
      .getOrElse("")

  /** Голос Брея в подвале: Элиас — свой, Элорин — свой; если живы оба, Брей —
    * Элорин, подмастерье сидит отдельно. */
  def breyVoice(survivors: Survivors): String = survivors match {
    case Survivors.Elias => Survivors.Elias.suffix
    case _               => Survivors.Elorin.suffix
  }

  /** Глава начинается дома, наутро после кладбища: сцена своего пути (у Тени —
    * записка того, кто пережил пролог), ведущий предмет — в сумку, если его там
    * нет, глава взята. Не пора — ничего. */
  def atHome(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository, content: SceneContent,
             user: User, renderer: Renderer): Task[Unit] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      ZIO.foreachDiscard(path(quests).filter(_ => canStart(quests))) { p =>
        for {
          now       <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
          hero      <- heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
                         .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
          survivors <- StoryLine.survivorsOf(heroDao, user.userId)
          _         <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, lead(p))
          taken      = quests.take(NpcQuest.StoryMorning, now)
          _         <- NpcQuestLog.save(heroDao, user.userId, taken)
          keys       = p match {
                         case Path.Shadow => List("story.storyMorning.start.shadow",
                                               s"story.storyMorning.start.note${survivors.suffix}",
                                               "story.storyMorning.start.shadowEnd")
                         case other       => List(s"story.storyMorning.start.${other.key}")
                       }
          _         <- renderer.show(user, Screen(
                         (keys.map(k => content.text(k).trim) :+ StoryLine.takenLine(content, NpcQuest.StoryMorning, taken))
                           .mkString("\n\n"), Nil))
        } yield ()
      }
    }

  /** Тень: хозяин «Трёх костей» и вышибалы. */
  def denFoes(owner: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, Race.Human, OwnerRarity, Some(owner)) ::
      BouncerRaces.map(race => MonsterGenerator.generateOfRaceAndRarity(Level, race, BouncerRarity))

  /** Щит и одиночка: именной главарь и его люди. */
  def gang(leader: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, FoeRace, LeaderRarity, Some(leader)) ::
      List.fill(Henchmen)(MonsterGenerator.generateOfRaceAndRarity(Level, FoeRace, HenchmanRarity))

  /** Гость в строю героя: имя, уровень задания и статы моба этой редкости;
    * место ему раздаёт [[withGuests]], после боя в отряд он не встаёт. */
  def guest(kind: AllyKind, name: String, rarity: Rarity): BattleAlly = {
    val stats = MonsterGenerator.generateOfRaceAndRarity(Level, GuestRace, rarity).fightStats
    BattleAlly(kind, position = 0, hp = stats.hp, armor = stats.armor, energy = stats.energy,
      stats = stats, lvl = Level.toLong, undead = Some(UndeadForm(name, Level.toLong, stats)), home = 0)
  }

  /** Щит: Горн и двое его искателей. */
  def cellarGuests(content: SceneContent): List[BattleAlly] =
    guest(AllyKind.GuildMaster, content.text("story.storyMorning.cellar.hornName"), HornRarity) ::
      content.list("story.storyMorning.cellar.seekers").map(name => guest(AllyKind.GuildSeeker, name, SeekerRarity))

  /** Одиночка: Барти со сковородой. */
  def bartiGuest(content: SceneContent): BattleAlly =
    guest(AllyKind.Informant, content.text("story.storyMorning.barti.name"), BartiRarity)

  /** Поставить гостей в строй — каждого на ближайшее к герою свободное место.
    * Отряд занял всё — кому места нет, тот бьётся из-за спин и в строй не встаёт. */
  def withGuests(battle: SoloPveBattle, guests: List[BattleAlly]): SoloPveBattle =
    guests.foldLeft(battle)((b, g) =>
      b.group.nearestFreeAllyPlace.fold(b)(pos =>
        b.copy(group = b.group.copy(allies = b.group.allies :+ g.copy(position = pos)))))

  /** Развязка главы — одна на все пути. `lead` — рассказ пути до описи (его
    * собрала сцена), `choices` — кнопки последнего сообщения. У Тени поручение
    * уходит из сумки, у всех — опись урожая в сумку, серебро и опыт; глава
    * закрыта, глава 7 взята и висит до обновления. */
  def finish(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository, content: SceneContent,
             user: User, hero: Hero, p: Path, lead: String, choices: List[Choice], renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      _     <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StoryShadowOrder))
      _     <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StoryHarvest)
      paid   = hero.copy(silver = hero.silver + silver)
      _     <- heroDao.updateSilver(user.userId, paid.silver)
      done  <- NpcQuestDialog(heroDao, content, NpcQuest.StoryMorning, "StoryMorning")
                 .complete(user, paid, _.take(NpcQuest.StoryNewcomer, now))
      (_, expLine) = done
      quests <- NpcQuestLog.load(heroDao, user.userId)
      harvest = content.format("story.storyMorning.harvest", "name" -> hero.name.getOrElse(""))
      _     <- renderer.show(user, Screen(
                 List(lead, harvest, content.text(s"story.storyMorning.coda.${p.key}")).map(_.trim).mkString("\n\n"), Nil))
      _     <- renderer.show(user, Screen(List(
                 content.format(s"story.storyMorning.reward.${p.key}", "silver" -> silver.toString, "exp" -> expLine),
                 content.text("story.storyMorning.questDone"),
                 StoryLine.takenLine(content, NpcQuest.StoryNewcomer, quests)).mkString("\n\n"), choices))
    } yield ()
}
