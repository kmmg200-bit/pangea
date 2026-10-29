package pangea.service

import zio.stm.TRef
import zio.{Duration, Exit, Promise, Scope, Task, UIO, ZIO, ZLayer, durationInt}

/** Фоновая работа, которой не место в ответе на запрос: ход игрока после «ok»
  * ВК, доставка передачи, письмо о донате, циклы планировщика и сверок.
  *
  * Раньше всё это шло через `forkDaemon`, и остановка сервера обрывала его где
  * придётся: пул базы закрывался прямо под ходом, JVM добивала остальное, а ВК,
  * получивший «ok», событие уже не повторял. Теперь остановка ([[shutdown]])
  * будит циклы, чтобы новых витков не начинали, ждёт начатое — до `grace` — и
  * только недождавшееся прерывает. Всё это — пока база и клиенты ещё открыты:
  * `Main` зовёт [[shutdown]] до того, как закроются слои. */
final class Background private (
  scope:    Scope.Closeable,
  running:  TRef[Int],
  stopping: Promise[Nothing, Unit],
  grace:    Duration
) {

  /** Запустить в фоне. При остановке дело дожидаются. */
  def fork(work: Task[Unit]): UIO[Unit] =
    ZIO.uninterruptibleMask { restore =>
      running.update(_ + 1).commit *>
        restore(work).ensuring(running.update(_ - 1).commit).forkIn(scope).unit
    }

  /** Запустить в фоне долгий обход, который безопасно оборвать на любом шаге,
    * а повтор доделает оставшееся: при остановке его не ждут, а прерывают сразу. */
  def forkUntilShutdown(work: Task[Unit]): UIO[Unit] =
    fork(work.raceFirst(stopping.await))

  /** Цикл: виток, затем пауза `every`. При остановке начатый виток доводится до
    * конца, а пауза обрывается — новых витков нет. Поломка витка — в лог под
    * `failed`, цикл идёт дальше. */
  def repeat(every: Duration, failed: String)(tick: Task[Unit]): UIO[Unit] =
    fork(
      (tick.catchAllCause(ErrorLog.logged(failed)) *> stopping.await.timeout(every))
        .repeatUntil(_.isDefined)
        .unit)

  /** Остановка: циклы — без новых витков, начатое — дождаться (не дольше
    * `grace`), что не успело — прервать.
    *
    * Зовётся из финализатора уже прерванного главного fiber'а, а там всё
    * непрерываемое. Таймаут же гонит ожидание в своём fiber'е и по истечении
    * прерывает его — так что прерываемым делаем только само ожидание, внутри
    * гонки. Снаружи `timeout` ожидание оборвалось бы сразу: прерывание главного
    * fiber'а уже ждёт первого прерываемого места; без `interruptible` вовсе —
    * таймаут не сработал бы никогда (см. BackgroundSpec). */
  def shutdown: UIO[Unit] =
    for {
      _     <- stopping.succeed(())
      count <- running.get.commit
      _     <- ZIO.logInfo(s"Остановка: дожидаюсь фоновых дел — $count")
      idle  <- running.get.retryUntil(_ == 0).commit.interruptible.timeout(grace)
      left  <- running.get.commit
      _     <- ZIO.when(idle.isEmpty)(ZIO.logWarning(s"Остановка: за $grace не закончились фоновые дела ($left) — прерываю"))
      _     <- scope.close(Exit.unit)
      _     <- ZIO.logInfo("Остановка: фон остановлен")
    } yield ()
}

object Background {

  /** Сколько остановка ждёт начатого. Ход и партия планировщика — секунды; а
    * Docker между SIGTERM и SIGKILL даёт `stop_grace_period` (docker-compose):
    * его хватает и на это, и на закрытие HTTP-сервера. */
  val Grace: Duration = 15.seconds

  def make(grace: Duration): UIO[Background] =
    for {
      // Недождавшееся прерываем все разом, а не по одному.
      scope    <- Scope.parallel
      running  <- TRef.make(0).commit
      stopping <- Promise.make[Nothing, Unit]
    } yield new Background(scope, running, stopping, grace)

  val live: ZLayer[Any, Nothing, Background] = ZLayer(make(Grace))
}
