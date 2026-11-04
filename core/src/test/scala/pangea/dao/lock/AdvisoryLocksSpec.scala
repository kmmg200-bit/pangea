package pangea.dao.lock

import pangea.dao.config.PostgresConfig
import pangea.model.user.UserId
import pangea.service.Leader
import pangea.service.state.PlayerLock
import zio.test._
import zio.{Promise, Ref, Scope, Task, ZIO, ZLayer, durationInt}

import java.sql.DriverManager

/** Локи между ботами на настоящем Postgres. Без базы не запускается: задать
  * `PANGEA_TEST_PG` — JDBC-адрес пустой базы (пользователь `PANGEA_TEST_PG_USER`,
  * по умолчанию test). Каждый «бот» — свой пул, то есть свои сессии. */
object AdvisoryLocksSpec extends ZIOSpecDefault {

  private def config = PostgresConfig(
    jdbcUrl      = sys.env.getOrElse("PANGEA_TEST_PG", ""),
    username     = sys.env.getOrElse("PANGEA_TEST_PG_USER", "test"),
    password     = sys.env.getOrElse("PANGEA_TEST_PG_PASSWORD", "x"),
    lockPoolSize = 4
  )

  /** Ещё один бот: свой пул локов, живёт до конца scope. */
  private val bot: ZIO[Scope, Throwable, AdvisoryLocks] =
    (ZLayer.succeed(config) >>> AdvisoryLocks.live).build.map(_.get[AdvisoryLocks])

  /** Сколько advisory-локов с ключом `key` держат сессии базы сейчас. */
  private def held(key: Long): Task[Int] = ZIO.attemptBlocking {
    val c = DriverManager.getConnection(config.jdbcUrl, config.username, config.password)
    try {
      val st = c.prepareStatement(
        "select count(*) from pg_locks where locktype = 'advisory' and granted and ((classid::bigint << 32) | objid::bigint) = ?")
      st.setLong(1, key)
      val rs = st.executeQuery(); rs.next(); rs.getInt(1)
    } finally c.close()
  }

  /** Убить сессии, что держат лок `key`, — как будто бот упал. */
  private def kill(key: Long): Task[Unit] = ZIO.attemptBlocking {
    val c = DriverManager.getConnection(config.jdbcUrl, config.username, config.password)
    try {
      val st = c.prepareStatement(
        "select pg_terminate_backend(pid) from pg_locks where locktype = 'advisory' and ((classid::bigint << 32) | objid::bigint) = ?")
      st.setLong(1, key); st.executeQuery(); ()
    } finally c.close()
  }

  override def spec = suite("Локи между ботами (Postgres)")(

    test("один игрок у двух ботов — по очереди; разные игроки — разом") {
      ZIO.scoped {
        for {
          a     <- bot.flatMap(PlayerLock.make)
          b     <- bot.flatMap(PlayerLock.make)
          log   <- Ref.make(List.empty[String])
          inA   <- Promise.make[Nothing, Unit]
          first  = a.withLock(UserId(1L))(inA.succeed(()) *> log.update(_ :+ "a-start") *> ZIO.sleep(300.millis) *> log.update(_ :+ "a-end"))
          f     <- first.fork
          _     <- inA.await
          _     <- b.withLock(UserId(1L))(log.update(_ :+ "b"))
          _     <- f.join
          order <- log.get
          // разные игроки: b входит, пока a держит своего
          busy  <- Promise.make[Nothing, Unit]
          g     <- a.withLock(UserId(2L))(busy.succeed(()) *> ZIO.sleep(2.seconds)).fork
          _     <- busy.await
          start <- zio.Clock.nanoTime
          _     <- b.withLock(UserId(3L))(ZIO.unit)
          took  <- zio.Clock.nanoTime.map(t => (t - start) / 1000000L)
          _     <- g.interrupt
        } yield assertTrue(order == List("a-start", "a-end", "b"), took < 1000L)
      }
    },

    test("соединение уходит в пул без лока: и после хода, и после падения, и после прерывания") {
      ZIO.scoped {
        for {
          a       <- bot
          _       <- a.withLock(10L)(ZIO.unit)
          _       <- a.withLock(11L)(ZIO.fail(new RuntimeException("ход упал"))).ignore
          inside  <- Promise.make[Nothing, Unit]
          f       <- a.withLock(12L)(inside.succeed(()) *> ZIO.never).fork
          _       <- inside.await
          during  <- held(12L)
          _       <- f.interrupt
          after   <- ZIO.foreach(List(10L, 11L, 12L))(held)
        } yield assertTrue(during == 1, after == List(0, 0, 0))
      }
    },

    test("бот упал (сессию убили) — его лок отпущен, второй бот проходит") {
      ZIO.scoped {
        for {
          a      <- bot
          b      <- bot
          inside <- Promise.make[Nothing, Unit]
          _      <- a.withLock(20L)(inside.succeed(()) *> ZIO.never).fork
          _      <- inside.await
          _      <- kill(20L)
          got    <- b.withLock(20L)(ZIO.succeed(true)).timeout(5.seconds)
        } yield assertTrue(got.contains(true))
      }
    },

    test("ведущий из двух — один; первый остановился — ведущим становится второй") {
      for {
        scopeA  <- Scope.make
        leaderA <- scopeA.extend[Any](bot.flatMap(l => (ZLayer.succeed(l) >>> Leader.live).build).map(_.get[Leader]))
        _       <- leaderA.isLeader.repeatUntil(identity).timeout(10.seconds)
        result  <- ZIO.scoped {
                     for {
                       leaderB <- bot.flatMap(l => (ZLayer.succeed(l) >>> Leader.live).build).map(_.get[Leader])
                       _       <- ZIO.sleep(1.second)
                       both    <- leaderA.isLeader.zip(leaderB.isLeader)
                       _       <- scopeA.close(zio.Exit.unit)
                       took    <- leaderB.isLeader.repeatUntil(identity).timed.map(_._1.toMillis)
                     } yield (both, took)
                   }
        (both, took) = result
      } yield assertTrue(both == (true, false), took <= Leader.Retry.toMillis + 2000L)
    } @@ TestAspect.timeout(30.seconds)
  ) @@ TestAspect.ifEnvSet("PANGEA_TEST_PG") @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
}
