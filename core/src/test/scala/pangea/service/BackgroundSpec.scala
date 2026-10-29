package pangea.service

import zio.test._
import zio.{Promise, Ref, ZIO, durationInt}

/** Фон и остановка сервера: начатое дожидаются, циклы новых витков не
  * начинают, зависшее прерывают по истечении grace. Время здесь настоящее. */
object BackgroundSpec extends ZIOSpecDefault {

  /** Как в `ZIOAppDefault`: SIGTERM прерывает главный fiber, остановка фона —
    * в его финализаторе. Возвращает fiber самого прерывания. */
  private def sigterm(bg: Background) =
    for {
      main <- ZIO.never.ensuring(bg.shutdown).fork
      stop <- main.interrupt.fork
    } yield stop

  override def spec = suite("Фон и остановка")(

    test("остановка ждёт начатое дело, пока то не закончится") {
      for {
        bg    <- Background.make(5.seconds)
        gate  <- Promise.make[Nothing, Unit]
        done  <- Ref.make(false)
        _     <- bg.fork(gate.await *> done.set(true))
        stop  <- bg.shutdown.fork
        _     <- ZIO.sleep(100.millis)
        early <- stop.poll
        _     <- gate.succeed(())
        _     <- stop.join
        ok    <- done.get
      } yield assertTrue(early.isEmpty && ok)
    },

    test("из финализатора прерванного главного fiber'а — ждёт так же, а не обрывает сразу") {
      for {
        bg    <- Background.make(5.seconds)
        gate  <- Promise.make[Nothing, Unit]
        done  <- Ref.make(false)
        _     <- bg.fork(gate.await *> done.set(true))
        stop  <- sigterm(bg)
        _     <- ZIO.sleep(100.millis)
        early <- stop.poll
        _     <- gate.succeed(())
        _     <- stop.join
        ok    <- done.get
      } yield assertTrue(early.isEmpty && ok)
    },

    test("зависшее дело прерывается, когда grace вышел, — и из финализатора тоже") {
      for {
        bg  <- Background.make(200.millis)
        cut <- Promise.make[Nothing, Unit]
        _   <- bg.fork(ZIO.never.onInterrupt(cut.succeed(())).unit)
        _   <- sigterm(bg).flatMap(_.join)
        was <- cut.isDone
      } yield assertTrue(was)
    },

    test("цикл: пауза между витками остановку не держит") {
      for {
        bg     <- Background.make(5.seconds)
        ticks  <- Ref.make(0)
        ticked <- Promise.make[Nothing, Unit]
        _      <- bg.repeat(1.hour, "виток упал")(ticks.update(_ + 1) *> ticked.succeed(()).unit)
        _      <- ticked.await
        _      <- bg.shutdown
        n      <- ticks.get
      } yield assertTrue(n == 1)
    },

    test("цикл: начатый виток доводится до конца, нового нет") {
      for {
        bg     <- Background.make(5.seconds)
        ticks  <- Ref.make(0)
        inTick <- Promise.make[Nothing, Unit]
        gate   <- Promise.make[Nothing, Unit]
        _      <- bg.repeat(1.milli, "виток упал")(inTick.succeed(()) *> gate.await *> ticks.update(_ + 1))
        _      <- inTick.await
        stop   <- bg.shutdown.fork
        _      <- ZIO.sleep(100.millis)
        early  <- stop.poll
        _      <- gate.succeed(())
        _      <- stop.join
        _      <- ZIO.sleep(50.millis)
        n      <- ticks.get
      } yield assertTrue(early.isEmpty && n == 1)
    },

    test("цикл: ошибка и падение витка — в лог, цикл идёт дальше") {
      for {
        bg    <- Background.make(5.seconds)
        calls <- Ref.make(0)
        third <- Promise.make[Nothing, Unit]
        _     <- bg.repeat(1.milli, "виток упал")(calls.updateAndGet(_ + 1).flatMap {
                   case 1 => ZIO.fail(new RuntimeException("ошибка"))
                   case 2 => ZIO.die(new RuntimeException("падение"))
                   case _ => third.succeed(()).unit
                 })
        _     <- third.await
        _     <- bg.shutdown
      } yield assertCompletes
    },

    test("обход до остановки обрывается сразу, grace не ждёт") {
      for {
        bg  <- Background.make(1.hour)
        cut <- Promise.make[Nothing, Unit]
        _   <- bg.forkUntilShutdown(ZIO.never.onInterrupt(cut.succeed(())).unit)
        _   <- bg.shutdown
        was <- cut.isDone
      } yield assertTrue(was)
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds)
}
