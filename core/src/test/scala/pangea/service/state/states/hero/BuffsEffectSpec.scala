package pangea.service.state.states.hero

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.{AzatState, Hero, LoreData, WeaponCoat, WeaponDust}
import pangea.model.home.HomeRates
import pangea.model.item.{BrewRates, MaterialKind}
import pangea.model.monster.Race
import pangea.model.stats.{ParamsBuff, StatBoost, StatBoosts}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.miniapp.MiniAppView
import pangea.service.state.UserAction
import pangea.service.state.states.gustavo.BoostStat
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.test._

/** «Зелья и бафы» в «Эффектах»: всё временное, что действует на герое, по
  * строке со сроком; нет ничего — нет и кнопки. */
object BuffsEffectSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val min      = 60L * 1000L
  private val now      = 1_000_000_000L

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private lazy val content: SceneContent = SceneContent.load()

  private def boost(name: String, leftMs: Long, buff: ParamsBuff = ParamsBuff.zero): StatBoost =
    StatBoost(name, buff, now + leftMs)

  /** Герой, на котором действует всё сразу. */
  private val buffed: Hero = TestFixtures.hero(userId).copy(
    statBoosts = StatBoosts(List(
      boost(BoostStat.Str.boostName, 34 * min, BoostStat.Str.buff),
      boost("herb:agi", 60 * min, ParamsBuff(0, 0, BrewRates.BoostPct, 0)),
      boost(HomeRates.RestedBoost, 120 * min),
      boost(BrewRates.WolfCallBoost, 30 * min),
      StatBoost(BrewRates.BubbleBoost, ParamsBuff.zero, Long.MaxValue),
      boost(BrewRates.SentenceBoost + Race.Orc.entryName, 10 * min),
      boost("someday:new", 5 * min),
      // выдохся — его нет
      boost(BoostStat.Agi.boostName, -1 * min, BoostStat.Agi.buff)
    )),
    expFrozen  = true,
    weaponDust = WeaponDust(
      layers  = List(MaterialKind.RubyDust, MaterialKind.RubyDust, MaterialKind.SapphireDust),
      coat    = Some(WeaponCoat.Poison),
      mirrors = 2)
  )

  override def spec = suite("Зелья и бафы")(

    test("каждое действующее — строкой со сроком; у зовов и пузыря — только название") {
      val lines = SkillsState.buffs(content, buffed, now, Some(now + 3L * 24L * 60L * min)).map(_.lines)
      assertTrue(lines.contains(List(
        "✨ Благословение Азата — ещё 3д 0ч",
        "🧪 Зелье быка: сила +15% — ещё 34мин",
        "🧪 Настой ясного глаза: ловкость +10% — ещё 1ч 0мин",
        s"🛏 Выспались дома: +${HomeRates.RestedExpPct}% опыта — ещё 2ч 0мин",
        "🧪 Волчий зов — ещё 30мин",
        "🧪 Пузырьковый нектар — действует",
        s"🧪 Зелье приговора: ${Race.Orc} — ещё 10мин",
        "🧪 Особый эффект — ещё 5мин",
        "❄ Зелье застоя: опыт остановлен",
        "🪄 Пыль на оружии: Рубиновая пыль ×2, Сапфировая пыль — до конца следующего боя",
        "🟢 Оружие смазано ядом — на ближайший бой",
        "🪞 Призрачных копий: 2 — на ближайший бой"
      )))
    },

    test("ничего не действует — нет и кнопки; благословение кончилось — его нет") {
      val plain = TestFixtures.hero(userId)
      assertTrue(SkillsState.buffs(content, plain, now, None).isEmpty) &&
      assertTrue(SkillsState.buffs(content, plain, now, Some(now - 1L)).isEmpty) &&
      assertTrue(!SkillsState.entries(plain).exists(_.isInstanceOf[SkillsState.SkillEntry.Buffs]))
    },

    test("на экране «Эффекты» — первой кнопкой «🧪 Зелья и бафы», в ней — строки; благословение берётся из Храма") {
      val hero = TestFixtures.hero(userId).copy(
        statBoosts = StatBoosts(List(StatBoost(BoostStat.Vit.boostName, BoostStat.Vit.buff, Long.MaxValue))))
      for {
        dao  <- TestHeroDao.withHero(userId, hero)
        _    <- dao.writeAzatData(userId, AzatState.empty.copy(blessingUntil = Some(Long.MaxValue)).asJson)
        r    <- TestRenderer.make
        state = SkillsState(dao, content)
        _    <- state.enter(testUser, r)
        list <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("Fx_Buffs"), r)
        card <- r.sentScreens.map(_.last)
      } yield assertTrue(list.choices.headOption.exists(c => c.id == "Fx_Buffs" && c.label == "🧪 Зелья и бафы")) &&
              assertTrue(card.text.startsWith("🧪 Зелья и бафы\n\n✨ Благословение Азата — ещё ")) &&
              assertTrue(card.text.contains("🧪 Зелье медоеда: телосложение +15% — ещё "))
    },

    test("Mini App: «Зелья и бафы» — первым эффектом, своим видом") {
      val view = MiniAppView.character(buffed, AzatState.empty, LoreData.empty, content, now)
      assertTrue(view.effects.headOption.exists(e => e.kind == "buffs" && e.label == "🧪 Зелья и бафы")) &&
      assertTrue(view.effects.headOption.exists(_.text.contains("❄ Зелье застоя: опыт остановлен")))
    }
  )
}
