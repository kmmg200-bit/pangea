package pangea.service.state.states.story

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.BattleAlly
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.stats.FightStats
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.{HedgeSongQuest, StoryClearingQuest, StoryCollectorQuest, StoryFeedQuest, StoryFurnaceQuest,
  StoryListQuest, StoryMorningQuest, StoryToolQuest}
import zio.ZIO
import zio.test._

/** Союзники героя в сюжетных боях Акта II — на ступень слабее, чем были (так
  * решил пользователь), кроме Барти и его людей. */
object StoryAlliesSpec extends ZIOSpecDefault {

  private def stats(lvl: Int, race: Race, rarity: Rarity): FightStats =
    MonsterGenerator.generateOfRaceAndRarity(lvl, race, rarity).fightStats

  /** Путь выбран в главе 5; `sojka` — в главе 4 Сойку простили, и на Рубеже II
    * она на крыше. */
  private def on(path: Path, sojka: Boolean = false): NpcQuests = {
    val chose = StoryToolQuest.finish(NpcQuests.empty.take(NpcQuest.StoryTool, 0L), path)
    if (!sojka) chose
    else chose.take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList)
      .update(NpcQuest.StoryList)(_.copy(counter = StoryListQuest.SojkaFate.Forgave.n))
  }

  private def strength(allies: List[BattleAlly]): List[(AllyKind, FightStats)] = allies.map(a => (a.kind, a.stats))

  override def spec = suite("Союзники в сюжете Акта II")(

    test("на ступень слабее: Горн и Э. — мифические, искатели и асессор — необычные, Сойка — редкая, Элиар — мифический") {
      for {
        content <- ZIO.attempt(SceneContent.load())
      } yield {
        val human = Race.Human
        assertTrue(strength(StoryMorningQuest.cellarGuests(content)) == List(
          AllyKind.GuildMaster -> stats(StoryMorningQuest.Level, human, Rarity.Mythical),
          AllyKind.GuildSeeker -> stats(StoryMorningQuest.Level, human, Rarity.Uncommon),
          AllyKind.GuildSeeker -> stats(StoryMorningQuest.Level, human, Rarity.Uncommon))) &&
        assertTrue(strength(StoryClearingQuest.guests(content, on(Path.Shield))) == List(
          AllyKind.GuildMaster -> stats(StoryClearingQuest.Level, human, Rarity.Mythical))) &&
        assertTrue(strength(StoryClearingQuest.guests(content, on(Path.Alone))) == List(
          AllyKind.HedgeElf -> stats(StoryClearingQuest.Level, Race.Elf, Rarity.Mythical))) &&
        assertTrue(strength(StoryFurnaceQuest.guests(content, on(Path.Shield))) == List(
          AllyKind.GuildMaster  -> stats(StoryFurnaceQuest.Level, human, Rarity.Mythical),
          AllyKind.CouncilEnvoy -> stats(StoryFurnaceQuest.Level, human, Rarity.Uncommon))) &&
        assertTrue(strength(StoryCollectorQuest.guests(content, on(Path.Shadow))) == List(
          AllyKind.BrotherhoodAgent -> stats(StoryCollectorQuest.Level, human, Rarity.Mythical))) &&
        assertTrue(strength(StoryCollectorQuest.guests(content, on(Path.Shield))) == List(
          AllyKind.GuildMaster -> stats(StoryCollectorQuest.Level, human, Rarity.Mythical),
          AllyKind.GuildSeeker -> stats(StoryCollectorQuest.Level, human, Rarity.Uncommon),
          AllyKind.GuildSeeker -> stats(StoryCollectorQuest.Level, human, Rarity.Uncommon))) &&
        // Рубеж II: те же гости уровнем рубежа и Сойка на крыше
        assertTrue(strength(StoryFeedQuest.guests(content, on(Path.Shadow, sojka = true))) == List(
          AllyKind.BrotherhoodAgent -> stats(StoryFeedQuest.Level, human, Rarity.Mythical),
          AllyKind.Sojka            -> stats(StoryFeedQuest.Level, human, Rarity.Rare)))
      }
    },

    test("Барти и его люди — как были: Барти необычный, капитаны каравана редкие; Элиар у изгороди легендарный") {
      for {
        content <- ZIO.attempt(SceneContent.load())
      } yield {
        val captains = StoryFurnaceQuest.guests(content, on(Path.Alone))
        assertTrue(strength(List(StoryMorningQuest.bartiGuest(content))) ==
          List(AllyKind.Informant -> stats(StoryMorningQuest.Level, Race.Human, Rarity.Uncommon))) &&
        assertTrue(strength(StoryCollectorQuest.guests(content, on(Path.Alone))) ==
          List(AllyKind.Informant -> stats(StoryCollectorQuest.Level, Race.Human, Rarity.Uncommon))) &&
        assertTrue(strength(StoryFeedQuest.guests(content, on(Path.Alone))) ==
          List(AllyKind.Informant -> stats(StoryFeedQuest.Level, Race.Human, Rarity.Uncommon))) &&
        assertTrue(captains.nonEmpty && strength(captains).forall { case (kind, s) =>
          kind == AllyKind.CaravanKhajiit && s == stats(StoryFurnaceQuest.Level, Race.Khajiit, Rarity.Rare) }) &&
        assertTrue(HedgeSongQuest.elf.stats == stats(HedgeSongQuest.Level, Race.Elf, Rarity.Legendary))
      }
    }
  )
}
