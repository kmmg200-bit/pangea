package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemType, QuestItemKind, Rarity}
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.ZIO
import zio.test._

/** Текст карточки вещи — общий у сумки бота и Mini App: у сюжетного предмета
  * — его описание, а не «Нет характеристик». */
object InventoryCardTextSpec extends ZIOSpecDefault {

  private val hero = TestFixtures.hero(UserId(1L))

  private val helmet: Item =
    Item(5L, "Шлем", 2L, Rarity.White, ItemType.Helmet,
      attack = 0, accuracy = 0, energy = 0, armor = 3, defence = 0, evasion = 0)

  override def spec = suite("Карточка вещи")(

    test("сюжетный предмет — описание; у карты Мурлоков — и сложность") {
      for {
        content <- ZIO.attempt(SceneContent.load())
        text     = InventoryState.cardText(content, QuestItemKind.item(QuestItemKind.MurlocVillageMap), hero, letterOpened = false)
      } yield assertTrue(
        text.contains(QuestItemKind.MurlocVillageMap.description),
        text.contains("Сложность"),
        !text.contains("Нет характеристик")
      )
    },

    test("невскрытое письмо — с подсказкой, вскрытое — без") {
      for {
        content <- ZIO.attempt(SceneContent.load())
        letter   = QuestItemKind.item(QuestItemKind.MarisaLetter)
        sealed_  = InventoryState.cardText(content, letter, hero, letterOpened = false)
        opened   = InventoryState.cardText(content, letter, hero, letterOpened = true)
        hint     = content.text("marisa.letterHint")
      } yield assertTrue(
        sealed_.contains(QuestItemKind.MarisaLetter.description) && sealed_.contains(hint),
        opened.contains(QuestItemKind.MarisaLetter.description) && !opened.contains(hint)
      )
    },

    test("обычная вещь — характеристики и сравнение с надетым, как прежде") {
      for {
        content <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(
        InventoryState.cardText(content, helmet, hero, letterOpened = false) ==
          InventoryState.itemText(helmet, hero.equipment, Some(hero))
      )
    }
  )
}
