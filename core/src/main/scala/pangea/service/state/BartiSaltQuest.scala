package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.BattleAlly
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.service.state.StoryToolQuest.Path

/** «Соль» — побочное задание Барти в Акте II, только у пути «Сам по себе»
  * ([[NpcQuest.BartiSalt]], задание 39-го уровня). Из накладной на лес герой
  * знает слово «Загон», а Барти знает, кто туда возит: Загон стоит на старой
  * солеварне, и соль туда возит караван Та'Нахира — каравану по пути, место в
  * повозке найдётся. Отсюда одиночка входит в главу 9.
  *
  * Берётся у Барти — с карточки накладной или кнопкой «Дом Барти» с Торговой
  * площади, — когда «Просека» закрыта. Шаг один: ночью к каравану у Сухого
  * колодца; на соляной колее караван встречает дозор Загона — к печи чужих не
  * возят, — бой: капитаны Ра'Тхар и Ма'Кесс в строю. Отбились — Та'Нахир платит
  * за охрану соли и повезёт героя к печи, когда шум уляжется. Враги, сложность
  * и награда — по уровню задания, а не героя. */
object BartiSaltQuest {

  /** Уровень задания: по нему сложность, дозор и капитаны. */
  val Level: Int = 39

  /** Ключ сюжетного боя на соляной колее: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "bartiSalt"

  /** Дозор Загона: именной старший и четверо — люди уровня задания. */
  val PatrolRace: Race      = Race.Human
  val LeaderRarity: Rarity  = Rarity.Mythical
  val PatrolRarity: Rarity  = Rarity.Rare
  val Patrol: Int           = 4

  /** Сколько дублонов Та'Нахир платит за охрану соли — так решил пользователь. */
  val Doubloons: Long = 50L

  private def alone(q: NpcQuests): Boolean = StoryToolQuest.choice(q).contains(Path.Alone)

  /** Задание можно взять: путь «Сам по себе», «Просека» закрыта, «Соли» ещё не было. */
  def available(q: NpcQuests): Boolean =
    alone(q) && q.isDone(NpcQuest.StoryClearing) && !q.isTaken(NpcQuest.BartiSalt) && !q.isDone(NpcQuest.BartiSalt)

  /** Ночью — к каравану у Сухого колодца. */
  def toCaravan(q: NpcQuests): Boolean = q.onStep(NpcQuest.BartiSalt, 1)

  /** Дозор Загона: старший — первым, именной, и четверо. */
  def foes(leader: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, PatrolRace, LeaderRarity, Some(leader)) ::
      List.fill(Patrol)(MonsterGenerator.generateOfRaceAndRarity(Level, PatrolRace, PatrolRarity))

  /** Капитаны каравана в строю — уровня задания. */
  def captains(content: SceneContent): List[BattleAlly] =
    content.list("barti.caravan.kinCaptains").map(BigDealQuest.captainAt(_, Level))

  /** Строка карточки накладной у одиночки: спросить Барти, ехать к каравану
    * или — после «Соли» — ждать, когда караван тронется к печи. Не одиночка
    * или «Просека» не закрыта — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!alone(q) || !q.isDone(NpcQuest.StoryClearing)) ""
    else if (available(q)) content.text("barti.salt.card.ask")
    else if (toCaravan(q)) content.text("barti.salt.card.caravan")
    else if (q.isDone(NpcQuest.BartiSalt)) content.text("barti.salt.card.done")
    else ""
}
