package pangea.service.state.states.battle

import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, SoloPveBattle}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.stats.FightStats
import pangea.model.user.UserId
import pangea.model.village.{VillageContract, VillageQuest}
import pangea.service.state.{HedgeSongQuest, StoryMorningQuest, Villages}
import pangea.test.TestFixtures
import zio.ZIO
import zio.test._

/** Союзники дерутся по своей расе, как мобы: эльф стреляет на три места —
  * освобождённый пленник, караванщик эльфийской деревни, Элиар (поднятые — в
  * RisenReachSpec); прочие достают соседей. */
object AllyRaceReachSpec extends ZIOSpecDefault {

  private val orc =
    Monster(0L, 10L, Race.Orc, Rarity.Common,
      FightStats(atk = 1, hp = 100, armor = 0, defence = 0, evasion = 0, accuracy = 0, energy = 0))

  private val hero = TestFixtures.hero(UserId(1L)).copy(lvl = 10L, squad = Squad(heroPos = 1))

  /** Кого бьёт с места 4 союзник боя, когда моб один — на месте 1 (в паре с героем). */
  private def targetOf(ally: BattleAlly): Option[Int] = {
    val b      = SoloPveBattle.fromGroup(List(orc), hero, Nil)
    val placed = ally.copy(position = 4)
    b.copy(group = b.group.copy(allies = List(placed))).allyTarget(placed)
  }

  /** То же — союзник из отряда героя, как его ставит в бой отряд. */
  private def fromSquad(ally: Ally): Option[Int] = {
    val b = SoloPveBattle.fromGroup(List(orc), hero.copy(squad = Squad(heroPos = 1, allies = List(ally.copy(position = 4)))), Nil)
    b.group.allies.headOption.flatMap(b.allyTarget)
  }

  private def contract(race: Race): VillageContract = VillageContract(VillageQuest.CaravanEscort, race, 1, 10, 500L)

  def spec = suite("Союзники дерутся по своей расе")(
    test("освобождённый пленник-эльф стреляет с места 4 по мобу на месте 1, пленник-орк оттуда не достаёт") {
      for {
        c      <- ZIO.attempt(SceneContent.load())
        elf     = Villages.freedCaptive(c, contract(Race.Elf), Race.Elf)
        orcForm = Villages.freedCaptive(c, contract(Race.Elf), Race.Orc)
      } yield assertTrue(
        elf.race.contains(Race.Elf.entryName),
        fromSquad(Ally(AllyKind.FreedCaptive, 4, 100L, 100L, 0L, undead = Some(elf))).contains(1),
        fromSquad(Ally(AllyKind.FreedCaptive, 4, 100L, 100L, 0L, undead = Some(orcForm))).isEmpty
      )
    },
    test("караванщик эльфийской деревни стреляет, караванщик людской деревни бьёт вблизи") {
      assertTrue(
        targetOf(Villages.caravanGuard(contract(Race.Elf), "Возница")).contains(1),
        targetOf(Villages.caravanGuard(contract(Race.Human), "Возница")).isEmpty
      )
    },
    test("Элиар стреляет; Фаэлин — как стрелял; наёмник-человек и Горн бьют вблизи") {
      assertTrue(
        targetOf(HedgeSongQuest.elfAt(10)).contains(1),
        fromSquad(Ally(AllyKind.Elf, 4, 100L, 100L, 0L)).contains(1),
        fromSquad(Ally(AllyKind.Human, 4, 100L, 100L, 0L)).isEmpty,
        targetOf(StoryMorningQuest.guest(AllyKind.GuildMaster, "Горн", Rarity.Legendary, 10)).isEmpty
      )
    }
  )
}
