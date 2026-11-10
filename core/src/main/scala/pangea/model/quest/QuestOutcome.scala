package pangea.model.quest

import enumeratum._

/** Чем герой закончил задание с развилкой. Лежит в `quest_outcomes` строкой
  * (`entryName`) рядом с ключом задания (`questKey` — [[NpcQuest.key]] или
  * [[FirstSteps.Key]]): значения не переименовывать и не удалять. По этим
  * записям после задания считается, сколько героев прошли его тем же путём.
  *
  * Отслеживаются задания, где выбор и правда разный: «Письмо Марисе»,
  * «Деревня Мурлоков», «Пропавшая дружина» — и «Встать на ноги»: в каком доме
  * герой его закончил. */
sealed abstract class QuestOutcome(override val entryName: String, val questKey: String) extends EnumEntry {

  /** Где в `scenes.yaml` название задания. */
  def titleKey: String =
    if (questKey == FirstSteps.Key) "firstSteps.title" else s"journal.quests.$questKey.title"
}

object QuestOutcome extends Enum[QuestOutcome] {
  val values: IndexedSeq[QuestOutcome] = findValues

  // ── «Письмо Марисе» ─────────────────────────────────────────────────────────

  /** Долг Кельвина уплачен за Марису: коллектору при ней — или ей самой после
    * боя. «Спаситель Марисы». */
  case object MarisaSaved extends QuestOutcome("saved", NpcQuest.Marisa.key)

  /** Коллектор повержен при Марисе, а её серебро герой оставил себе. «Мерзавец». */
  case object MarisaAbandoned extends QuestOutcome("abandoned", NpcQuest.Marisa.key)

  /** За тайником — без Марисы; от коллектора откупился. */
  case object MarisaPaidAlone extends QuestOutcome("paidAlone", NpcQuest.Marisa.key)

  /** За тайником — без Марисы; коллектора одолел. */
  case object MarisaKilledAlone extends QuestOutcome("killedAlone", NpcQuest.Marisa.key)

  /** Пал от руки коллектора — с Марисой или без. */
  case object MarisaFell extends QuestOutcome("fell", NpcQuest.Marisa.key)

  // ── «Деревня Мурлоков» ──────────────────────────────────────────────────────

  /** Довёз двадцать единиц снаряжения. «Любимец Мурлоков». */
  case object MurlocHelped extends QuestOutcome("helped", NpcQuest.Murloc.key)

  /** Вырезал деревню, ничего ей не сдав. «Гроза Мурлоков». */
  case object MurlocRaided extends QuestOutcome("raided", NpcQuest.Murloc.key)

  /** Сначала носил снаряжение, потом вырезал деревню. «Гроза Мурлоков». */
  case object MurlocBetrayed extends QuestOutcome("betrayed", NpcQuest.Murloc.key)

  // ── «Пропавшая дружина» ─────────────────────────────────────────────────────

  /** Вывел дружину наверх. «Друг подгорного народа». */
  case object SquadLedOut extends QuestOutcome("ledOut", NpcQuest.LostSquad.key)

  /** Бросил дружину у фонаря и унёс её груз Гуннару. «Расхититель штолен». */
  case object SquadLooted extends QuestOutcome("looted", NpcQuest.LostSquad.key)

  // ── «Встать на ноги»: в каком доме засчитано (строка — House.key) ─────────────

  /** «Домик в две комнаты». */
  case object FirstStepsCottage extends QuestOutcome("cottage", FirstSteps.Key)

  /** «Добротный дом». */
  case object FirstStepsTownhouse extends QuestOutcome("townhouse", FirstSteps.Key)

  /** Дом на острове. */
  case object FirstStepsIsland extends QuestOutcome("island", FirstSteps.Key)

  /** Исходы задания — в порядке объявления. Пусто — задание не отслеживается. */
  def of(questKey: String): List[QuestOutcome] = values.filter(_.questKey == questKey).toList

  def of(quest: NpcQuest): List[QuestOutcome] = of(quest.key)

  /** Исход по записи из базы: задание и строка исхода. */
  def find(questKey: String, name: String): Option[QuestOutcome] = of(questKey).find(_.entryName == name)

  def find(quest: NpcQuest, name: String): Option[QuestOutcome] = find(quest.key, name)
}
