package pangea.service.state.states.battle

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.model.battle.{SlainMonster, SoloPveBattle}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.squad.{Ally, AllyKind, Squad, UndeadForm}
import pangea.model.stats.FightStats
import pangea.model.user.UserId
import pangea.service.state.states.events.cave.DarkAltar
import pangea.test.TestFixtures
import zio.test._

/** Поднятые — с алтаря и набором «Некромант» — дерутся, как при жизни: эльф
  * стреляет на три места, как эльфы-мобы, прочие достают соседей. Раса
  * записана в форме поднятого; у поднятых раньше её нет — узнаём по имени. */
object RisenReachSpec extends ZIOSpecDefault {

  private val orc =
    Monster(0L, 10L, Race.Orc, Rarity.Common,
      FightStats(atk = 1, hp = 100, armor = 0, defence = 0, evasion = 0, accuracy = 0, energy = 0))

  /** Бой с одним мобом на месте 1 (в паре с героем) и поднятым `form` на месте 4. */
  private def standing(form: UndeadForm): SoloPveBattle = {
    val h = TestFixtures.hero(UserId(1L)).copy(lvl = 10L,
      squad = Squad(heroPos = 1, allies = List(Ally(AllyKind.Undead, 4, 100L, 100L, 0L, undead = Some(form)))))
    SoloPveBattle.fromGroup(List(orc), h, Nil)
  }

  private def target(form: UndeadForm): Option[Int] = {
    val b = standing(form)
    b.group.allies.headOption.flatMap(b.allyTarget)
  }

  def spec = suite("Поднятые дерутся, как при жизни")(
    test("эльф с алтаря стреляет: с места 4 достаёт моба на месте 1; орк с алтаря оттуда не достаёт никого") {
      val elf     = DarkAltar.fromTrophy(Race.Elf, Rarity.Uncommon, 10L)
      val orcForm = DarkAltar.fromTrophy(Race.Orc, Rarity.Uncommon, 10L)
      assertTrue(
        elf.race.contains(Race.Elf.entryName) && elf.raceOf.contains(Race.Elf),
        orcForm.race.contains(Race.Orc.entryName),
        target(elf).contains(1),
        target(orcForm).isEmpty
      )
    },
    test("набором «Некромант» встаёт и названный враг: имя его, раса записана — эльф стреляет") {
      val slain = SlainMonster(10L, Race.Elf.entryName, Rarity.Legendary.entryName, marked = false, name = "Кто-то Безымянный")
      val form  = BattleState.riseForm(slain, None)
      assertTrue(
        form.exists(f => f.name == "Кто-то Безымянный" && f.raceOf.contains(Race.Elf)),
        form.flatMap(target).contains(1)
      )
    },
    test("поднятый до правки — в записи нет расы: узнаём по имени, «Эльф лучник» стреляет, Тёмный волк — нет") {
      val stats = FightStats(atk = 10, hp = 100, armor = 0, defence = 0, evasion = 0, accuracy = 0, energy = 0)
      // Так форму писали раньше: имя, уровень и статы, без расы.
      def old(name: String): Json = Json.obj("name" -> name.asJson, "lvl" -> 10L.asJson, "stats" -> stats.asJson)
      val archer = old(Monster.namesByRaceRarity((Race.Elf, Rarity.Uncommon))).as[UndeadForm]
      val marked = old(s"${Monster.UndeadPrefix}-${Monster.MarkedPrefix} ${Monster.namesByRaceRarity((Race.Elf, Rarity.Rare))}")
                     .as[UndeadForm]
      val wolf   = old(UndeadForm.DarkWolfName).as[UndeadForm]
      assertTrue(
        archer.exists(f => f.race.isEmpty && f.raceOf.contains(Race.Elf)),
        archer.toOption.flatMap(target).contains(1),
        marked.exists(_.raceOf.contains(Race.Elf)),
        wolf.exists(_.raceOf.isEmpty),
        wolf.toOption.flatMap(target).isEmpty
      )
    },
    test("раса пишется в форму и читается обратно; без расы — поля в записи нет") {
      val elf  = DarkAltar.fromTrophy(Race.Elf, Rarity.Rare, 12L)
      val bare = elf.copy(race = None)
      assertTrue(
        elf.asJson.hcursor.get[String]("race").toOption.contains(Race.Elf.entryName),
        elf.asJson.as[UndeadForm].toOption.contains(elf),
        bare.asJson.hcursor.downField("race").failed,
        bare.asJson.as[UndeadForm].toOption.contains(bare)
      )
    }
  )
}
