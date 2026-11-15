package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.StoryToolQuest.Path
import zio.Task

/** «Крыша» — побочное задание Барти в Акте II ([[NpcQuest.BartiRoof]], задание
  * 28-го уровня). На каджитское золото Барти открыл лавку на Торговой площади,
  * и ночная стража берёт с неё плату «за охрану». Берётся в лавке — кнопкой с
  * Торговой площади, пока задание доступно или идёт: Рубеж I пройден и
  * «Большое дело» закрыто. В сумку ложится квитанция стражи — на её карточке,
  * что делать дальше.
  *
  * У каждого пути Рубежа I свой первый шаг ([[firstStep]]): Щит несёт
  * квитанцию Юноне (1) — без поличного она капитана не тронет, но её люди
  * будут за углом; Тени Братство предлагает решить вопрос само (2): согласился
  * — без боя, но теперь Барти должен Братству, отказался — разбираться самому;
  * одиночка — сразу ночь в лавке (3). Ночью приходит капитан ночной стражи с
  * четырьмя — бой; Барти прячется под прилавком. Враги, сложность и награда —
  * по уровню задания, а не героя. */
object BartiRoofQuest {

  /** Уровень задания: по нему сложность, ночная стража и награда. */
  val Level: Int = 28

  /** Ключ сюжетного боя в лавке: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "bartiRoof"

  /** Ночная стража: именной капитан и четверо — люди уровня задания. */
  val GuardRace: Race       = Race.Human
  val CaptainRarity: Rarity = Rarity.Mythical
  val GuardRarity: Rarity   = Rarity.Rare
  val Guards: Int           = 4

  /** Барти платит из выручки — серебром за уровень задания. */
  val SilverPerLvl: Long = 150L

  def silver: Long = SilverPerLvl * Level

  /** Как решили у Тени: номер — в `counter` закрытого задания. */
  sealed abstract class Deal(val n: Long, val key: String)

  object Deal {
    /** Пусть решает Братство: без боя, а Барти теперь должен Братству. */
    case object Brotherhood extends Deal(1L, "brotherhood")
    /** Разобраться самому: ночью в лавке, бой со стражей. */
    case object Self extends Deal(2L, "self")

    val all: List[Deal] = List(Brotherhood, Self)
  }

  /** Задание можно взять: Рубеж I пройден, «Большое дело» закрыто, «Крыша»
    * ещё не взята и не закрыта. */
  def available(q: NpcQuests): Boolean =
    q.isDone(NpcQuest.StoryTool) && q.isDone(NpcQuest.BigDeal) && !q.isTaken(NpcQuest.BartiRoof) && !q.isDone(NpcQuest.BartiRoof)

  /** Кнопка «Лавка Барти» на Торговой площади: задание доступно или идёт. */
  def shopOpen(q: NpcQuests): Boolean = available(q) || q.isTaken(NpcQuest.BartiRoof)

  def toJunona(q: NpcQuests): Boolean = q.onStep(NpcQuest.BartiRoof, 1)
  def toOffer(q: NpcQuests): Boolean  = q.onStep(NpcQuest.BartiRoof, 2)
  def toNight(q: NpcQuests): Boolean  = q.onStep(NpcQuest.BartiRoof, 3)

  /** С карточки квитанции — в лавку: Тени — за ответом Братству, всем — ночью. */
  def toShop(q: NpcQuests): Boolean = toOffer(q) || toNight(q)

  /** Первый шаг — по пути Рубежа I: Щит идёт к Юноне, Тени предлагает
    * Братство, одиночка (и герой без записанного пути) — сразу ночью в лавку. */
  def firstStep(q: NpcQuests): Int = StoryToolQuest.choice(q) match {
    case Some(Path.Shield) => 1
    case Some(Path.Shadow) => 2
    case _                 => 3
  }

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.BartiRoof)(_.copy(step = n))

  /** Взять задание — сразу на первом шаге своего пути. */
  def take(q: NpcQuests, now: Long): NpcQuests = step(q.take(NpcQuest.BartiRoof, now), firstStep(q))

  /** Как решили у Тени — у закрытого задания. */
  def deal(q: NpcQuests): Option[Deal] =
    Option.when(q.isDone(NpcQuest.BartiRoof))(q.of(NpcQuest.BartiRoof).counter).flatMap(n => Deal.all.find(_.n == n))

  /** Строка карточки квитанции — что делать сейчас. Задание не идёт — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isTaken(NpcQuest.BartiRoof)) ""
    else content.list("barti.roof.card.steps").lift(q.of(NpcQuest.BartiRoof).step - 1).getOrElse("")

  /** Ночная стража: капитан — первым, именной, и четверо. */
  def foes(captain: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, CaptainRarity, Some(captain)) ::
      List.fill(Guards)(MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, GuardRarity))

  /** Развязка: квитанция уходит из сумки, серебро и опыт, задание закрыто — у
    * Тени с тем, как решили. Ответ — строка награды, затея Барти на будущее и
    * «Квест завершён». */
  def finish(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent, user: User, hero: Hero,
             deal: Option[Deal]): Task[String] =
    for {
      _    <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.BartiReceipt))
      paid  = hero.copy(silver = hero.silver + silver)
      _    <- heroDao.updateSilver(user.userId, paid.silver)
      done <- NpcQuestDialog(heroDao, content, NpcQuest.BartiRoof, "BartiRoof")
                .complete(user, paid, q => deal.fold(q)(d => q.update(NpcQuest.BartiRoof)(_.copy(counter = d.n))))
      (_, expLine) = done
    } yield List(
      content.format("barti.roof.reward", "silver" -> silver.toString, "exp" -> expLine),
      content.text("barti.roof.next").trim,
      content.text("barti.roof.questDone")).mkString("\n\n")
}
