package pangea.model.vk

import io.circe.parser.parse
import zio.test._

/** Ключи от повторов и маскировка текста в хранимом событии. */
object InboxEventSpec extends ZIOSpecDefault {

  private def json(s: String) = parse(s).fold(throw _, identity)

  override def spec = suite("Событие очереди ВК")(

    test("ключ сообщения — peer, id и cmid: личка и беседа различаются") {
      assertTrue(
        InboxEvent.messageKey(7L, 100L, 0L) == "m:7:100:0",
        InboxEvent.messageKey(2000000001L, 0L, 33710L) == "m:2000000001:0:33710"
      )
    },

    test("ключ события группы — event_id; без него — по содержимому, одинаковое к одинаковому") {
      val liked = json("""{"type":"like_add","event_id":"abc","object":{}}""")
      val noId1 = json("""{"type":"like_add","object":{"liker_id":1}}""")
      val noId2 = json("""{"type":"like_add","object":{"liker_id":1}}""")
      val noId3 = json("""{"type":"like_add","object":{"liker_id":2}}""")
      assertTrue(
        InboxEvent.groupKey(liked) == "e:abc",
        InboxEvent.groupKey(noId1) == InboxEvent.groupKey(noId2),
        InboxEvent.groupKey(noId1) != InboxEvent.groupKey(noId3),
        InboxEvent.groupKey(noId1).startsWith("e:sha256:")
      )
    },

    test("маска — только текст сообщения; событие без сообщения — как было") {
      val msg   = json("""{"type":"message_new","object":{"message":{"text":"pass","peer_id":7}}}""")
      val group = json("""{"type":"like_add","object":{"liker_id":1}}""")
      val f     = (t: String) => if (t == "pass") "***" else t
      assertTrue(
        InboxEvent.mapText(msg)(f).hcursor.downField("object").downField("message").get[String]("text") == Right("***"),
        InboxEvent.mapText(msg)(f).hcursor.downField("object").downField("message").get[Long]("peer_id") == Right(7L),
        InboxEvent.mapText(group)(f) == group
      )
    }
  )
}
