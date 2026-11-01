package pangea.dao.vk

import doobie.util.transactor
import zio.{Task, ZLayer}

/** Сообщения ВК, уже взятые в работу (`vk_seen_messages`). Одно сообщение
  * приходит двумя путями — Callback API и Long Poll, — и ход по нему должен
  * случиться один раз, каким бы путём оно ни пришло первым. */
trait SeenMessageDao {

  /** Отметить сообщение; `true` — его ещё не было и оно наше, `false` — его
    * уже взяли по другому пути. Двое одновременно — выиграет один: решает
    * первичный ключ, а не проверка перед записью. */
  def firstTime(peerId: Long, messageId: Long, conversationMessageId: Long, now: Long): Task[Boolean]

  /** Забыть отмеченные раньше `before` (epoch ms); возвращает, сколько забыто. */
  def forget(before: Long): Task[Int]
}

object SeenMessageDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, SeenMessageDaoLive] =
    ZLayer.fromFunction(new SeenMessageDaoLive(_))
}
