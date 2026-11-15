package pangea.service.state.states.story

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.model.battle.{BattleAlly, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.monster.Monster
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.StoryMorningQuest
import pangea.service.state.states.LootState.LootData
import zio.{Random, Task, ZIO}

/** Сюжетный бой главы Акта II: враги, отряд с героем и гости в строю, без
  * «Сбежать»; добыча вернёт в сцену с отметкой победы — у каждой сцены свой
  * ключ: в `scene_data` лежат и чужие структуры. */
object StoryFight {

  /** Записать бой: стартовая энергия врагов — как у всех мобов, гости — на
    * ближайших к герою свободных местах ([[StoryMorningQuest.withGuests]]),
    * `story` — ключ боя (без «Сбежать», награда как за обычный), добыча
    * вернёт в `back` с отметкой `wonKey`. `kegs` — сколько бочек с порохом
    * стоит на поле (см. PowderKeg). Экран боя покажет сам бой. */
  def start(heroDao: HeroDao, user: User, hero: Hero, foes: List[Monster], guests: List[BattleAlly], story: String,
            back: StateType, wonKey: String, kegs: Int = 0): Task[Unit] =
    for {
      energies <- ZIO.foreach(foes)(m =>
                    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
      battle    = StoryMorningQuest.withGuests(
                    SoloPveBattle.fromGroup(foes, hero, energies).copy(story = Some(story), kegs = kegs), guests)
      routing   = LootData(Nil, Nil, returnState = Some(back), eventData = Some(mark(wonKey)))
      _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _        <- heroDao.writeSceneData(user.userId, routing.asJson)
    } yield ()

  /** Отметка победы под своим ключом. */
  def mark(wonKey: String): Json = Json.obj(wonKey -> true.asJson)

  /** Добыча вернула сюда с отметкой победы под этим ключом. */
  def won(data: Option[Json], wonKey: String): Boolean =
    data.exists(_.hcursor.get[Boolean](wonKey).toOption.contains(true))
}
