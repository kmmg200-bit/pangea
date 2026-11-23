package pangea.service.storage

import io.circe.Json
import pangea.engine.Journal
import pangea.generator.item.FlaskGenerator
import pangea.model.GameEvent
import pangea.model.item.{FlaskKind, Item, Rarity}
import pangea.model.user.UserId
import pangea.test.TestJournal
import zio.test._
import zio.{Task, ZIO}

/** След вещей в журнале: что именно пишется — номер, название, редкость, откуда
  * действие, — что пустое не пишется, а сломанный журнал не мешает игроку. */
object ItemTrailSpec extends ZIOSpecDefault {

  private val user = UserId(47L)

  private def flask(kind: FlaskKind, rarity: Rarity, id: Long): Item = FlaskGenerator.item(kind, rarity).copy(id = id)

  // Фиолетовая и пурпурная — обе 🟣: различить их можно только по редкости.
  private val windy   = flask(FlaskKind.Air, Rarity.Violet, 657740L)
  private val cleanse = flask(FlaskKind.Cleansing, Rarity.Purple, 669723L)

  private def ids(j: Json, field: String): List[Long] =
    j.hcursor.downField(field).values.toList.flatten.flatMap(_.hcursor.get[Long]("id").toOption)

  private def str(j: Json, field: String): Option[String] = j.hcursor.get[String](field).toOption

  private val broken: Journal = new Journal {
    def append(event: GameEvent): Task[Unit]                        = ZIO.fail(new Throwable("база недоступна"))
    def count(userId: UserId, eventType: String, sinceMs: Long): Task[Long] = ZIO.succeed(0L)
  }

  def spec = suite("ItemTrail: след вещей в журнале")(
    test("перенос и «Выбросить»: откуда, куда, вещи номерами, и что это приложение") {
      for {
        j    <- TestJournal.make
        trail = new ItemTrail(Some(j), ItemTrail.App)
        _    <- trail.moved(user, "bag", "wardrobe", List(cleanse))
        _    <- trail.dropped(user, "vault", List(windy))
        log  <- j.logged
      } yield {
        val moved   = log.head.payload
        val dropped = log(1).payload
        val first   = moved.hcursor.downField("items").downArray
        assertTrue(
          log.map(_.eventType) == List(GameEvent.ItemMoved, GameEvent.ItemDropped),
          log.forall(_.userId == user),
          str(moved, "via").contains("app"),
          str(moved, "from").contains("bag") && str(moved, "to").contains("wardrobe"),
          ids(moved, "items") == List(669723L),
          first.get[String]("name").toOption.contains(cleanse.name),
          first.get[String]("rarity").toOption.contains("Purple"),
          str(dropped, "from").contains("vault") && ids(dropped, "items") == List(657740L)
        )
      }
    },
    test("пояс: что легло и что сошло вместо него — с новыми номерами; снятое — отдельной записью") {
      val off = flask(FlaskKind.Cleansing, Rarity.Purple, 669723L)
      for {
        j    <- TestJournal.make
        trail = new ItemTrail(Some(j), ItemTrail.Bot)
        _    <- trail.beltPut(user, List(windy), Nil)
        _    <- trail.beltPut(user, List(cleanse.copy(id = 669722L, rarity = Rarity.Violet)), List(off))
        _    <- trail.beltTaken(user, List(windy.copy(id = 667887L)))
        log  <- j.logged
      } yield assertTrue(
        log.map(_.eventType) == List(GameEvent.BeltPut, GameEvent.BeltPut, GameEvent.BeltTaken),
        log.forall(e => str(e.payload, "via").contains("bot")),
        ids(log.head.payload, "put") == List(657740L) && ids(log.head.payload, "off").isEmpty,
        ids(log(1).payload, "put") == List(669722L) && ids(log(1).payload, "off") == List(669723L),
        ids(log(2).payload, "items") == List(667887L)
      )
    },
    test("нечего записать — записи нет; без журнала — тишина; журнал сломан — действие не падает") {
      for {
        j     <- TestJournal.make
        _     <- new ItemTrail(Some(j), ItemTrail.App).moved(user, "bag", "vault", Nil)
        _     <- new ItemTrail(Some(j), ItemTrail.Bot).beltPut(user, Nil, Nil)
        log   <- j.logged
        _     <- ItemTrail.silent.beltTaken(user, List(windy))
        quiet <- new ItemTrail(Some(broken), ItemTrail.App).moved(user, "bag", "vault", List(windy)).exit
      } yield assertTrue(log.isEmpty, quiet.isSuccess)
    }
  )
}
