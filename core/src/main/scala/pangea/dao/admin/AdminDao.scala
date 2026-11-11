package pangea.dao.admin

import doobie.implicits._
import doobie.util.transactor
import pangea.model.GameEvent
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.ItemSet
import pangea.model.quest.DailyRates
import pangea.model.rune.RuneData
import pangea.model.skill.Skill
import pangea.model.stats.BaseStats
import pangea.model.user.{UserId, VkId}
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Сводка по серверу для админ-панели.
  *
  * @param heroes      сколько героев заведено всего
  * @param activeToday сколько игроков заходило сегодня — с 00:00 по Москве (см. `users.last_seen_at`)
  * @param active7d    то же за неделю
  * @param heroSilver  серебро на руках у героев
  * @param vaultSilver серебро, лежащее в банковских ячейках
  * @param doubloons   дублоны на руках
  * @param levels      по каким уровням разошлись те, кто заходил за неделю
  * @param fetLottery     сколько раз сыграли в беспроигрышную лотерею Фета — всего
  * @param fetLottery7d   то же за неделю
  */
final case class AdminStats(
  heroes:       Long,
  activeToday:  Long,
  active7d:     Long,
  heroSilver:   Long,
  vaultSilver:  Long,
  doubloons:    Long,
  levels:       List[AdminStats.LevelRow],
  fetLottery:   Long = 0L,
  fetLottery7d: Long = 0L
) {
  def silverTotal: Long = heroSilver + vaultSilver
}

object AdminStats {
  /** «С 26 по 50 уровень — двенадцать живых героев». */
  final case class LevelRow(from: Long, to: Long, heroes: Long)

  val empty: AdminStats = AdminStats(0L, 0L, 0L, 0L, 0L, 0L, Nil)

  /** Разложить уровни по полосам шириной `step`: 1–25, 26–50 и так далее до
    * `maxLvl`. Пустые полосы остаются в списке — по ним видно, где никого нет. */
  def levelRows(levels: List[Long], step: Long, maxLvl: Long): List[LevelRow] = {
    val bands = (0L until ((maxLvl + step - 1) / step)).toList.map(i => (i * step + 1, ((i + 1) * step).min(maxLvl)))
    bands.map { case (from, to) =>
      LevelRow(from, to, levels.count(l => l >= from && l <= to).toLong)
    }
  }

}

/** Игровая сводка для админ-панели — по тем, кто заходил за неделю, как уровни
  * в [[AdminStats]]: брошенные герои первого уровня только размыли бы картину.
  * Всё — снимок «как сейчас», а не история: кто выпил Зелье забвения, у того
  * очки в свободных, пока не раскидает их заново; кто снял меч — его умения в
  * счёте нет.
  *
  * @param heroes   сколько героев в счёте
  * @param sets     кто в наборах: набор, сколько его предметов надето, сколько таких героев
  * @param invested сколько очков вложено в каждую характеристику — суммой по героям
  * @param free     сколько очков ещё не распределено
  * @param skills   активные умения на героях: у скольких есть и у скольких из них — клеймом
  */
final case class GameStats(
  heroes:   Long,
  sets:     List[GameStats.SetRow],
  invested: BaseStats,
  free:     Long,
  skills:   List[GameStats.SkillRow]
)

object GameStats {
  /** «54 игрока держат набор Охотника на шести предметах». */
  final case class SetRow(set: ItemSet, worn: Int, heroes: Long)

  /** «Хитрый удар — у 34 героев, у пятерых из них клеймом на теле». */
  final case class SkillRow(skill: Skill, heroes: Long, onBody: Long)

  /** Один герой в счёте: что надето, характеристики, свободные очки, клейма. */
  final case class HeroRow(equipment: Equipment, base: BaseStats, free: Long, runes: RuneData)

  val Zero: BaseStats  = BaseStats(0L, 0L, 0L, 0L)
  val empty: GameStats = GameStats(0L, Nil, Zero, 0L, Nil)

  def of(rows: List[HeroRow], minWorn: Int): GameStats =
    GameStats(
      heroes   = rows.size.toLong,
      sets     = setRows(rows.map(_.equipment), minWorn),
      invested = invested(rows.map(_.base)),
      free     = rows.map(_.free.max(0L)).sum,
      skills   = skillRows(rows.map(r => r.equipment -> r.runes)))

  /** Кто сколько носит: по каждому набору — сколько героев держат столько-то
    * его предметов. Один предмет набором не считается: это ещё не
    * «придерживаться». Считаем в памяти, а не в SQL: экипировка лежит в JSONB
    * одним объектом, и разбирать её запросом было бы куда сложнее, чем взять
    * готовую [[Equipment.setCounts]]. */
  def setRows(all: List[Equipment], minWorn: Int): List[SetRow] =
    all.flatMap(_.setCounts.filter(_._2 >= minWorn))
      .groupBy(identity)
      .map { case ((set, worn), xs) => SetRow(set, worn, xs.size.toLong) }
      .toList
      .sortBy(r => (r.set.entryName, -r.worn))

  /** Сколько очков вложено в каждую характеристику — суммой по героям. В
    * характеристиках героя лежат только старт ([[Hero.StartBaseStats]]) и
    * вложенное: раса, зелья, достижения и наборы ложатся сверху при подсчёте
    * (см. `Hero.effectiveBaseStats`), — значит, вложено всё, что выше старта.
    * На том же стоит Зелье забвения (см. `Hero.spentPoints`). */
  def invested(all: List[BaseStats]): BaseStats = {
    val s = Hero.StartBaseStats
    all.foldLeft(Zero) { (acc, b) =>
      BaseStats(
        agi = acc.agi + (b.agi - s.agi).max(0L),
        vit = acc.vit + (b.vit - s.vit).max(0L),
        str = acc.str + (b.str - s.str).max(0L),
        int = acc.int + (b.int - s.int).max(0L))
    }
  }

  /** Активные умения на героях — ровно те, что работают в бою
    * ([[Hero.activeSkillSlotsOf]]): по умению — у скольких героев оно есть и у
    * скольких из них выжжено клеймом на теле (хоть бы оно и повторяло вещь).
    * Частые — выше, поровну — по алфавиту. */
  def skillRows(all: List[(Equipment, RuneData)]): List[SkillRow] =
    all.flatMap { case (eq, runes) =>
      val branded = runes.brandedActives.toSet
      Hero.activeSkillSlotsOf(eq, runes).map(_.skill).distinct.map(s => s -> branded.contains(s))
    }.groupBy(_._1).toList
      .map { case (skill, xs) => SkillRow(skill, xs.size.toLong, xs.count(_._2).toLong) }
      .sortBy(r => (-r.heroes, r.skill.label))
}

trait AdminDao {
  def stats: Task[AdminStats]

  /** Игровая сводка: наборы, вложенные очки и активные умения — по тем, кто
    * заходил за неделю (см. [[GameStats]]). */
  def gameStats: Task[GameStats]

  /** Уровень прав в панели: 0 — обычный игрок, 1 — «Админ 1». */
  def level(userId: UserId): Task[Int]

  /** Выдать уровень; 0 и ниже — снять права совсем. */
  def setLevel(userId: UserId, level: Int): Task[Unit]

  /** Все, у кого уровень не ниже `min`, — с адресом в ВК: им пересылается /home. */
  def withLevel(min: Int): Task[List[(UserId, VkId)]]
}

/** Запросы админ-панели. Героев на сервере немного, а панель открывают руками
  * и редко, поэтому экипировку, характеристики и клейма читаем целиком и
  * считаем в памяти (см. [[GameStats.of]]). */
class AdminDaoLive(xa: transactor.Transactor[Task]) extends AdminDao {

  override def stats: Task[AdminStats] =
    for {
      heroes    <- sql"select count(*) from heroes".query[Long].unique.transact(xa)
      now       <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      // Сегодня — календарные сутки по Москве: с полуночи, а не последние 24 часа.
      day       <- activeFrom(DailyRates.dayOf(now) * DailyRates.DayMs - DailyRates.MoscowOffsetMs)
      week      <- activeSince("7 days")
      money     <- sql"select coalesce(sum(silver), 0), coalesce(sum(doubloons), 0) from heroes"
                     .query[(Long, Long)].unique.transact(xa)
      inVaults  <- sql"select coalesce(sum(silver), 0) from bank_vaults".query[Long].unique.transact(xa)
      // Уровни только живых: тех, кто заходил за неделю.
      alive     <- sql"""select h.lvl from heroes h
                         join users u on u.id = h.user_id
                         where u.last_seen_at > now() - interval '7 days'"""
                     .query[Long].to[List].transact(xa)
      // Лотерея Фета: каждая игра — строка журнала событий.
      lottery   <- sql"""select count(*),
                                count(*) filter (where occurred_at > now() - interval '7 days')
                         from event_log where event_type = ${GameEvent.FetRuneLottery}"""
                     .query[(Long, Long)].unique.transact(xa)
    } yield AdminStats(
      heroes      = heroes,
      activeToday = day,
      active7d    = week,
      heroSilver  = money._1,
      vaultSilver = inVaults,
      doubloons   = money._2,
      levels      = AdminStats.levelRows(alive, AdminDao.LevelStep, AdminDao.MaxLevel),
      fetLottery   = lottery._1,
      fetLottery7d = lottery._2)

  // Те же «живые», что у уровней: заходили за неделю.
  override def gameStats: Task[GameStats] =
    sql"""select h.equipment, h.base_stats, coalesce(h.upgrade_points, 0)::bigint, h.rune_data from heroes h
          join users u on u.id = h.user_id
          where u.last_seen_at > now() - interval '7 days'"""
      .query[(Equipment, BaseStats, Long, RuneData)].to[List].transact(xa)
      .map(rows => GameStats.of(rows.map { case (e, b, f, r) => GameStats.HeroRow(e, b, f, r) }, AdminDao.MinWorn))

  /** Сколько игроков заходило за этот срок. `interval` подставляется не
    * параметром, а строкой — но строку задаём здесь сами, снаружи она не
    * приходит. */
  private def activeSince(interval: String): Task[Long] =
    doobie.Fragment.const(s"select count(*) from users where last_seen_at > now() - interval '$interval'")
      .query[Long].unique.transact(xa)

  /** Сколько игроков заходило с момента `fromMs` (мс эпохи). `last_seen_at` —
    * TIMESTAMP без зоны: now() пишет туда время в зоне сессии, поэтому и границу
    * переводим в зону сессии — сравнение верно при любой зоне сервера базы. */
  private def activeFrom(fromMs: Long): Task[Long] =
    sql"""select count(*) from users
          where last_seen_at >= (to_timestamp(${fromMs.toDouble} / 1000.0) AT TIME ZONE current_setting('TimeZone'))"""
      .query[Long].unique.transact(xa)

  override def level(userId: UserId): Task[Int] =
    sql"select level from admins where user_id = ${userId.value}"
      .query[Int].option.transact(xa).map(_.getOrElse(0))

  override def setLevel(userId: UserId, level: Int): Task[Unit] =
    (if (level <= 0) sql"delete from admins where user_id = ${userId.value}"
     else sql"""insert into admins (user_id, level) values (${userId.value}, $level)
                on conflict (user_id) do update set level = excluded.level, granted_at = now()""")
      .update.run.transact(xa).unit

  override def withLevel(min: Int): Task[List[(UserId, VkId)]] =
    sql"""select u.id, u.vk_id from admins a
          join users u on u.id = a.user_id
          where a.level >= $min"""
      .query[(Long, String)].to[List].transact(xa)
      .map(_.map { case (id, vk) => (UserId(id), VkId(vk)) })

}

object AdminDao {
  /** Со скольких надетых предметов считаем, что герой держит набор. */
  val MinWorn: Int = 2

  /** Ширина полосы уровней в сводке и потолок, до которого их считать. */
  val LevelStep: Long = 25L
  val MaxLevel: Long  = 150L

  val live: ZLayer[transactor.Transactor[Task], Nothing, AdminDao] =
    ZLayer.fromFunction(new AdminDaoLive(_))
}
