package pangea.service

import zio.{Cause, IO, Trace, ZIO}

/** Как писать поломки в лог: причина (`Cause`) целиком — и ошибка, и падение
  * кода (defect), со стеком, — а не одно `getMessage`, которое у
  * NullPointerException и вовсе `null`. Прерывание — не поломка: его не пишем и
  * не глотаем, оно идёт дальше. */
object ErrorLog {

  /** Обработчик для `catchAllCause`: записать причину и идти дальше. */
  def logged[E](what: => String)(cause: Cause[E])(implicit trace: Trace): IO[E, Unit] =
    if (cause.isInterruptedOnly) ZIO.refailCause(cause)
    else ZIO.logErrorCause(what, cause)
}
