package pangea.dao.lock

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import pangea.dao.config.PostgresConfig
import pangea.metrics.GameMetrics
import zio.{Duration, Schedule, Task, UIO, ZIO, ZLayer, durationInt}

import java.sql.Connection

/** Advisory-локи Postgres — общие для всех процессов бота: пока бот один, хватало
  * локов в памяти, а два бота разом (выкладка «новый рядом со старым») должны
  * ждать друг друга.
  *
  * Лок живёт, пока жива сессия, поэтому держится на своём соединении из
  * отдельного маленького пула (см. [[AdvisoryLocksLive]]): бот упал — Postgres
  * отпускает его локи сам, вместе с соединением. Ключи: id игрока — для лока
  * игрока, отрицательные — служебные (см. `Leader`). */
trait AdvisoryLocks {

  /** Выполнить под локом `key`: занят другим процессом — ждём. */
  def withLock[A](key: Long)(action: Task[A]): Task[A]

  /** Держать лок `key`, пока не прервут. Нет его — пробовать раз в `retry`;
    * взяли — раз в `retry` проверять, что соединение живо. Взяли или потеряли
    * — `held(true)` / `held(false)`. Прервали — лок отпускается. */
  def hold(key: Long, retry: Duration)(held: Boolean => UIO[Unit]): UIO[Nothing]
}

object AdvisoryLocks {

  /** Без Postgres: лок только в процессе (его и так держит PlayerLock). Для
    * тестов. */
  val none: AdvisoryLocks = new AdvisoryLocks {
    def withLock[A](key: Long)(action: Task[A]): Task[A] = action
    def hold(key: Long, retry: Duration)(held: Boolean => UIO[Unit]): UIO[Nothing] = held(true) *> ZIO.never
  }

  /** Пул — `postgres.lock-pool-size` (POSTGRES_LOCK_POOL_SIZE): столько ходов
    * разом может идти под локом, следующий ждёт соединения. Занятость пула —
    * в метрику раз в секунду. */
  val live: ZLayer[PostgresConfig, Throwable, AdvisoryLocks] =
    ZLayer.scoped {
      for {
        config <- ZIO.service[PostgresConfig]
        ds     <- ZIO.acquireRelease(ZIO.attemptBlocking {
                    val hc = new HikariConfig()
                    hc.setJdbcUrl(config.jdbcUrl)
                    hc.setUsername(config.username)
                    hc.setPassword(config.password)
                    hc.setPoolName("advisory-locks")
                    hc.setMaximumPoolSize(config.lockPoolSize)
                    hc.setMinimumIdle(2)
                    new HikariDataSource(hc)
                  })(ds => ZIO.attemptBlocking(ds.close()).orDie)
        _      <- sample(ds, config.lockPoolSize).repeat(Schedule.spaced(1.second)).forkScoped
      } yield new AdvisoryLocksLive(ds)
    }

  /** Занятость пула — в метрику; пока пул не поднялся, молчим. */
  private def sample(ds: HikariDataSource, max: Int): UIO[Unit] =
    ZIO.foreachDiscard(Option(ds.getHikariPoolMXBean)) { pool =>
      ZIO.foreachDiscard(List(
        "active"  -> pool.getActiveConnections,
        "idle"    -> pool.getIdleConnections,
        "pending" -> pool.getThreadsAwaitingConnection,
        "max"     -> max
      )) { case (state, n) => GameMetrics.lockPool.tagged("state", state).set(n.toDouble) }
    }
}

final class AdvisoryLocksLive(ds: HikariDataSource) extends AdvisoryLocks {
  import AdvisoryLocksLive._

  override def withLock[A](key: Long)(action: Task[A]): Task[A] =
    ZIO.scoped {
      connection.flatMap(conn =>
        ZIO.attemptBlocking(tryLock(conn, key)).repeat(Waiting) *> action)
    }

  override def hold(key: Long, retry: Duration)(held: Boolean => UIO[Unit]): UIO[Nothing] =
    ZIO.scoped {
      for {
        conn <- connection
        _    <- ZIO.attemptBlocking(tryLock(conn, key)).repeat(Schedule.recurUntil[Boolean](identity) && Schedule.spaced(retry))
        _    <- ZIO.acquireRelease(held(true))(_ => held(false))
        n    <- (ZIO.sleep(retry) *> ZIO.attemptBlocking(ping(conn))).forever
      } yield n
    }.catchAllCause(cause =>
      ZIO.logWarningCause(s"Advisory-лок $key: соединение потеряно — пробую снова", cause) *> ZIO.sleep(retry)
    ).forever

  /** Соединение из пула на время scope. Возвращая его, снимаем все локи сессии:
    * пул соединение не закрывает, и лок, оставшийся на нём, жил бы вечно. */
  private val connection: ZIO[zio.Scope, Throwable, Connection] =
    ZIO.acquireRelease(ZIO.attemptBlocking(ds.getConnection))(conn =>
      ZIO.attemptBlocking(unlockAll(conn)).ignoreLogged *> ZIO.attemptBlocking(conn.close()).ignoreLogged)
}

object AdvisoryLocksLive {

  /** Ждать занятый лок: опрос с паузой от 5 мс до 100 мс. Занят он бывает
    * только тогда, когда тот же игрок в ходу у другого бота, — секунды выкладки. */
  private val Waiting: Schedule[Any, Boolean, Any] =
    Schedule.recurUntil[Boolean](identity) && (Schedule.exponential(5.millis) || Schedule.spaced(100.millis))

  private def tryLock(conn: Connection, key: Long): Boolean = query(conn, "select pg_try_advisory_lock(?)", Some(key))

  private def unlockAll(conn: Connection): Unit = { query(conn, "select pg_advisory_unlock_all() is null", None); () }

  private def ping(conn: Connection): Unit = { query(conn, "select true", None); () }

  private def query(conn: Connection, sql: String, key: Option[Long]): Boolean = {
    val st = conn.prepareStatement(sql)
    try {
      key.foreach(st.setLong(1, _))
      val rs = st.executeQuery()
      try { rs.next(); rs.getBoolean(1) } finally rs.close()
    } finally st.close()
  }
}
