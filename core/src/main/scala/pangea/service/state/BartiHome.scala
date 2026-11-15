package pangea.service.state

import pangea.model.hero.Hero
import pangea.model.quest.{NpcQuest, NpcQuests}

/** Дом Барти — его место на Торговой площади (так решил пользователь). После
  * «Большого дела» (караван каджитов) кнопка «Дом Барти» на площади стоит
  * насовсем, и все дела с Барти идут отсюда: в доме — экран с его заданиями
  * (кнопка на каждое, что можно взять или что уже идёт). Задания лавки —
  * «Крыша» и «Большая поставка» — тоже отсюда: кнопка ведёт в лавку. Своей
  * кнопки у лавки на площади больше нет.
  *
  * Пока Барти в «Долговой яме» (с 35-го уровня героя, см. BartiPitQuest), дом
  * опечатан: вместо экрана — синяя дверь под печатью и само задание. Глава
  * «Наутро» у Барти идёт раньше «Ямы», как и прежде.
  *
  * До «Большого дела» всё по-старому: в дом ведёт карточка адреса, а одиночку
  * — и «Соль» с площади. */
object BartiHome {

  /** Задание Барти на экране его дома: кнопка `id` с названием по ключу
    * `titleKey`. Лавочные ведут в лавку — одной кнопкой: «Поставка» идёт только
    * после «Крыши». */
  sealed abstract class Errand(val id: String, val titleKey: String)

  object Errand {
    /** Акт II, «Наутро», путь «Сам по себе»: Барти находит себя в книге ячейки. */
    case object Morning extends Errand("BartiQuestMorning", "journal.quests.storyMorning.title")
    case object Salt extends Errand("BartiQuestSalt", "journal.quests.bartiSalt.title")
    case object Door extends Errand("BartiQuestDoor", "journal.quests.bartiDoor.title")
    case object Roof extends Errand("BartiShop", "journal.quests.bartiRoof.title")
    case object Delivery extends Errand("BartiShop", "journal.quests.bartiDelivery.title")
  }

  /** Барти обжился: «Большое дело» закрыто — его дом открыт насовсем. */
  def settled(q: NpcQuests): Boolean = q.isDone(NpcQuest.BigDeal)

  /** Кнопка «Дом Барти» на Торговой площади: после «Большого дела» — всегда;
    * до него — только у одиночки, пока «Соль» ждёт разговора с Барти. */
  def onSquare(q: NpcQuests): Boolean = settled(q) || BartiSaltQuest.available(q)

  /** Барти дома и встречает экраном своих дел: «Большое дело» закрыто, и дом
    * не опечатан «Ямой». `story` — у Барти ждёт глава «Наутро»: она идёт раньше
    * «Ямы», дом при ней открыт. */
  def atHome(q: NpcQuests, hero: Hero, story: Boolean): Boolean =
    settled(q) && (story || !BartiPitQuest.houseOpen(q, hero))

  /** Задания на экране дома — по порядку кнопок: что можно взять или что уже
    * идёт. «Яма» сюда не попадает: при ней дом опечатан. */
  def errands(q: NpcQuests, story: Boolean): List[Errand] =
    List(
      Option.when(story)(Errand.Morning),
      Option.when(BartiSaltQuest.available(q) || BartiSaltQuest.toCaravan(q))(Errand.Salt),
      Option.when(BartiDoorQuest.houseOpen(q))(Errand.Door),
      if (BartiRoofQuest.shopOpen(q)) Some(Errand.Roof)
      else Option.when(BartiDeliveryQuest.shopOpen(q))(Errand.Delivery)
    ).flatten
}
