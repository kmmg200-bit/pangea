package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.user.User
import pangea.service.state.StoryToolQuest.Path
import zio.Task

/** «Синяя дверь» — побочное задание Барти в Акте II, только у пути «Сам по
  * себе» ([[NpcQuest.BartiDoor]], задание 33-го уровня). Барти похвастался,
  * с кем теперь водится, и Братство пометило его дверь: ночью придут жечь дом.
  * Жечь Братство наняло тех, кто жжёт лучше, — демонов: их удары поджигают
  * сами (огонь — оружие их расы). Отбиваются вдвоём: Барти со сковородой — в
  * строю. После — дом с дырами в ставнях становится убежищем героя (пока
  * только в сюжете).
  *
  * Берётся в доме Барти — кнопкой с экрана дома, пока задание доступно или
  * идёт: путь «Сам по себе», «Наутро» и «Большое дело» закрыты. Шаг один:
  * встретить ночью поджигателей (бой). Враги, сложность и награда — по уровню
  * задания, а не героя. */
object BartiDoorQuest {

  /** Уровень задания: по нему сложность, поджигатели и награда. */
  val Level: Int = 33

  /** Ключ сюжетного боя у синей двери: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "bartiDoor"

  /** Поджигатели: именной старший и четверо — демоны уровня задания (так решил
    * пользователь: «чтобы поджигали своими атаками»). */
  val FoeRace: Race          = Race.Demon
  val LeaderRarity: Rarity   = Rarity.Mythical
  val HenchmanRarity: Rarity = Rarity.Rare
  val Henchmen: Int          = 4

  /** Задаток Братства у старшего — серебром за уровень задания, как в «Крыше». */
  val SilverPerLvl: Long = 150L

  def silver: Long = SilverPerLvl * Level

  /** Задание можно взять: путь «Сам по себе», «Наутро» (Барти — партнёр) и
    * «Большое дело» закрыты, «Синяя дверь» ещё не взята и не закрыта. */
  def available(q: NpcQuests): Boolean =
    StoryToolQuest.choice(q).contains(Path.Alone) && q.isDone(NpcQuest.StoryMorning) && q.isDone(NpcQuest.BigDeal) &&
      !q.isTaken(NpcQuest.BartiDoor) && !q.isDone(NpcQuest.BartiDoor)

  /** Кнопка задания на экране дома Барти: задание доступно или идёт. */
  def houseOpen(q: NpcQuests): Boolean = available(q) || q.isTaken(NpcQuest.BartiDoor)

  /** Ночь у синей двери: ждать поджигателей. */
  def toNight(q: NpcQuests): Boolean = q.onStep(NpcQuest.BartiDoor, 1)

  /** Старший поджигатель — первым, именной, и четверо. */
  def foes(leader: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, FoeRace, LeaderRarity, Some(leader)) ::
      List.fill(Henchmen)(MonsterGenerator.generateOfRaceAndRarity(Level, FoeRace, HenchmanRarity))

  /** Развязка: серебро и опыт, задание закрыто. Ответ — строка награды и
    * «Квест завершён». */
  def finish(heroDao: HeroDao, content: SceneContent, user: User, hero: Hero): Task[String] =
    for {
      _    <- heroDao.updateSilver(user.userId, hero.silver + silver)
      done <- NpcQuestDialog(heroDao, content, NpcQuest.BartiDoor, "BartiDoor")
                .complete(user, hero.copy(silver = hero.silver + silver), identity)
      (_, expLine) = done
    } yield List(content.format("barti.door.reward", "silver" -> silver.toString, "exp" -> expLine),
      content.text("barti.door.questDone")).mkString("\n\n")
}
