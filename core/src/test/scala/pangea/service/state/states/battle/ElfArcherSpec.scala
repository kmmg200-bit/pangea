package pangea.service.state.states.battle

import pangea.model.battle.SoloPveBattle
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.stats.FightStats
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Фаэлин Тихострел — эльф-лучник из таверны: тоньше Плюха, бьёт издалека, как
  * лук героя, вблизи слабее, и через раз травит (сам яд — в AllyBattleSpec). */
object ElfArcherSpec extends ZIOSpecDefault {

  private val orc =
    Monster(0L, 10L, Race.Orc, Rarity.Common,
      FightStats(atk = 1, hp = 100, armor = 0, defence = 0, evasion = 0, accuracy = 0, energy = 0))

  /** Бой с одним мобом на месте 1 (в паре с героем) и союзником `kind` на месте 4. */
  private def standing(kind: AllyKind): SoloPveBattle = {
    val h = TestFixtures.hero(UserId(1L)).copy(lvl = 10L,
      squad = Squad(heroPos = 1, allies = List(Ally(kind, 4, 100L, 100L, 0L))))
    SoloPveBattle.fromGroup(List(orc), h, Nil)
  }

  def spec = suite("Фаэлин Тихострел")(
    test("тоньше Плюха и бьёт сильнее, растёт до десятого; стреляет на три места и травит через раз; стихии нет") {
      val elf   = AllyKind.Elf.stats(10L)
      val plukh = AllyKind.Murloc.stats(10L)
      assertTrue(
        elf == FightStats(atk = 350, hp = 700, armor = 600, defence = 200, evasion = 1700, accuracy = 1700, energy = 1000),
        AllyKind.Elf.stats(20L) == elf,
        elf.hp < plukh.hp && elf.armor < plukh.armor && elf.atk > plukh.atk && elf.evasion > plukh.evasion,
        AllyKind.Elf.reach == 3 && AllyKind.Elf.ranged && AllyKind.Elf.poisonPct == 50L,
        AllyKind.Murloc.reach == 1 && !AllyKind.Murloc.ranged && AllyKind.Murloc.poisonPct == 0L,
        AllyKind.Elf.element.isEmpty && AllyKind.Elf.race == Race.Elf,
        AllyKind.hireable == List(AllyKind.Human, AllyKind.Murloc, AllyKind.Gnome, AllyKind.Elf)
      )
    },
    test("с места 4 достаёт моба на месте 1; Йорген оттуда не достаёт никого") {
      val elf   = standing(AllyKind.Elf)
      val human = standing(AllyKind.Human)
      assertTrue(
        elf.group.allies.headOption.flatMap(elf.allyTarget).contains(1),
        human.group.allies.headOption.flatMap(human.allyTarget).isEmpty
      )
    },
    test("вблизи — по тому, кто напротив, и по соседям — бьёт на пятую часть слабее; дальше — в полную силу; у прочих штрафа нет") {
      val elf   = standing(AllyKind.Elf).group.allies.head
      val human = standing(AllyKind.Human).group.allies.head
      assertTrue(
        BattleState.allyCloseMod(elf, 4) == 0.8,
        BattleState.allyCloseMod(elf, 3) == 0.8,
        BattleState.allyCloseMod(elf, 5) == 0.8,
        BattleState.allyCloseMod(elf, 2) == 1.0,
        BattleState.allyCloseMod(elf, 1) == 1.0,
        BattleState.allyCloseMod(human, 4) == 1.0
      )
    }
  )
}
