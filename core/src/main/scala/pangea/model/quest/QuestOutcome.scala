package pangea.model.quest

import enumeratum._

/** Чем герой закончил задание с развилкой. Лежит в `quest_outcomes` строкой
  * (`entryName`) рядом с ключом задания ([[NpcQuest.key]]): значения не
  * переименовывать и не удалять. По этим записям после задания считается,
  * сколько героев прошли его тем же путём.
  *
  * Отслеживаются только задания, где выбор и правда разный: «Письмо Марисе»,
  * «Деревня Мурлоков», «Пропавшая дружина». */
sealed abstract class QuestOutcome(override val entryName: String, val quest: NpcQuest) extends EnumEntry

object QuestOutcome extends Enum[QuestOutcome] {
  val values: IndexedSeq[QuestOutcome] = findValues

  // ── «Письмо Марисе» ─────────────────────────────────────────────────────────

  /** Долг Кельвина уплачен за Марису: коллектору при ней — или ей самой после
    * боя. «Спаситель Марисы». */
  case object MarisaSaved extends QuestOutcome("saved", NpcQuest.Marisa)

  /** Коллектор повержен при Марисе, а её серебро герой оставил себе. «Мерзавец». */
  case object MarisaAbandoned extends QuestOutcome("abandoned", NpcQuest.Marisa)

  /** За тайником — без Марисы; от коллектора откупился. */
  case object MarisaPaidAlone extends QuestOutcome("paidAlone", NpcQuest.Marisa)

  /** За тайником — без Марисы; коллектора одолел. */
  case object MarisaKilledAlone extends QuestOutcome("killedAlone", NpcQuest.Marisa)

  /** Пал от руки коллектора — с Марисой или без. */
  case object MarisaFell extends QuestOutcome("fell", NpcQuest.Marisa)

  // ── «Деревня Мурлоков» ──────────────────────────────────────────────────────

  /** Довёз двадцать единиц снаряжения. «Любимец Мурлоков». */
  case object MurlocHelped extends QuestOutcome("helped", NpcQuest.Murloc)

  /** Вырезал деревню, ничего ей не сдав. «Гроза Мурлоков». */
  case object MurlocRaided extends QuestOutcome("raided", NpcQuest.Murloc)

  /** Сначала носил снаряжение, потом вырезал деревню. «Гроза Мурлоков». */
  case object MurlocBetrayed extends QuestOutcome("betrayed", NpcQuest.Murloc)

  // ── «Пропавшая дружина» ─────────────────────────────────────────────────────

  /** Вывел дружину наверх. «Друг подгорного народа». */
  case object SquadLedOut extends QuestOutcome("ledOut", NpcQuest.LostSquad)

  /** Бросил дружину у фонаря и унёс её груз Гуннару. «Расхититель штолен». */
  case object SquadLooted extends QuestOutcome("looted", NpcQuest.LostSquad)

  /** Исходы задания — в порядке объявления. Пусто — задание не отслеживается. */
  def of(quest: NpcQuest): List[QuestOutcome] = values.filter(_.quest == quest).toList

  /** Исход по записи из базы: задание и строка исхода. */
  def find(quest: NpcQuest, name: String): Option[QuestOutcome] = of(quest).find(_.entryName == name)
}
