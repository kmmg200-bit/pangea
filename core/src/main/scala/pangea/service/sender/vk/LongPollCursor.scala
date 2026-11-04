package pangea.service.sender.vk

import zio.Task

/** Где Long Poll остановился в очереди ВК (`ts`). Хранится вне процесса: после
  * перезапуска чтение продолжается с того же места, и всё, что пришло за
  * паузу, забирается сразу (см. [[VkLongPoll]]). */
trait LongPollCursor {
  def load: Task[Option[String]]
  def save(ts: String, now: Long): Task[Unit]
}
