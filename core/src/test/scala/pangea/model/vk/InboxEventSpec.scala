package pangea.model.vk

import io.circe.parser.parse
import zio.test._

/** Ключи от повторов и маскировка текста в хранимом событии. */
object InboxEventSpec extends ZIOSpecDefault {

  private def json(s: String) = parse(s).fold(throw _, identity)

  override def spec = suite("Событие очереди ВК")(

    test("ключ сообщения — peer, id и cmid: личка и беседа различаются; у основного сообщества — прежний") {
      assertTrue(
        InboxEvent.messageKey(Community.Main, 7L, 100L, 0L) == "m:7:100:0",
        InboxEvent.messageKey(Community.Main, 2000000001L, 0L, 33710L) == "m:2000000001:0:33710"
      )
    },

    test("беседа гильдейского сообщества с тем же номером и cmid — другой ключ: сообщения не глушат друг друга") {
      assertTrue(
        InboxEvent.messageKey(Community.Guilds, 2000000001L, 0L, 33710L) == "g:2000000001:0:33710",
        InboxEvent.messageKey(Community.Guilds, 2000000001L, 0L, 33710L) !=
          InboxEvent.messageKey(Community.Main, 2000000001L, 0L, 33710L)
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

    test("событие гильдейского сообщества — в обёртке: разворачивается в то же событие, основное — как есть") {
      val msg     = json("""{"type":"message_new","object":{"message":{"text":"Мой профиль","peer_id":2000000001}},"group_id":7}""")
      val wrapped = InboxEvent.wrapGuilds(msg)
      assertTrue(
        InboxEvent.unwrap(wrapped) == (Community.Guilds -> msg),
        InboxEvent.unwrap(msg) == (Community.Main -> msg)
      )
    },

    test("прежний разбор обёртку не принимает: сообщения в ней сверху не видно") {
      // Старая версия бота читает object.message с верхнего уровня — у обёртки
      // его нет, событие ей не разобрать, и в чужую беседу она не ответит.
      val wrapped = InboxEvent.wrapGuilds(json("""{"type":"message_new","object":{"message":{"text":"Передать","peer_id":2000000001}}}"""))
      assertTrue(
        wrapped.hcursor.downField("object").failed,
        wrapped.hcursor.downField("type").failed
      )
    },

    test("маска в обёрнутом событии — внутри обёртки, обёртка на месте") {
      val wrapped = InboxEvent.wrapGuilds(json("""{"type":"message_new","object":{"message":{"text":"pass","peer_id":7}}}"""))
      val masked  = InboxEvent.mapText(wrapped)(t => if (t == "pass") "***" else t)
      assertTrue(
        InboxEvent.unwrap(masked)._1 == Community.Guilds,
        InboxEvent.unwrap(masked)._2.hcursor.downField("object").downField("message").get[String]("text") == Right("***")
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
