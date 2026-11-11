package pangea.model.home

import pangea.model.item.Item

/** Куда в доме ложится вещь, пришедшая посылкой (см. [[HomeIntake]]). `key` —
  * её место в текстах письма (`parcel.places.*`). */
sealed abstract class HomePlace(val key: String)

object HomePlace {
  case object Runes extends HomePlace("runes")
  case object Herbs extends HomePlace("herbs")
  case object Brews extends HomePlace("brews")
  case object Gems  extends HomePlace("gems")

  /** Порядок в письме. */
  val ordered: List[HomePlace] = List(Runes, Herbs, Brews, Gems)
}

/** Что из пришедшего посылкой ложится в дом сразу — по тем же правилам, что
  * «Разложить всё»: рунные камни — на рунический алтарь (в любом доме, без
  * предела), травы и отвары — на полки верстака (со второго дома), камни — в
  * хранилище (на острове), пока хватает места. Стопки (📦), сюжетное и всё
  * прочее в дом не идут — им в банковскую ячейку, а не влезло — на почту.
  *
  * Здесь только расчёт: дом пишет и посылки забирает вызывающий
  * (см. `Parcels`). */
object HomeIntake {

  /** Дом после раскладки и что легло — по вещи, с местом. */
  final case class Result(home: HomeData, taken: List[(Item, HomePlace)])

  /** Разложить `items` по порядку: каждая следующая видит, сколько места
    * оставили прежние. */
  def of(home: HomeData, items: List[Item]): Result =
    items.foldLeft(Result(home, Nil)) { (acc, item) =>
      placeOf(acc.home, item).fold(acc)(place => Result(put(acc.home, item, place), acc.taken :+ (item -> place)))
    }

  /** Берёт ли дом такую вещь вообще — по одному её виду, без дома и мест:
    * рунный камень, трава, отвар или камень, не стопка и не сюжетное. Нет
    * таких среди посылок — дом можно и не читать. */
  def wanted(item: Item): Boolean =
    !item.isBundle && !item.isQuestItem && !item.isStoryGear &&
      (item.runeStone.isDefined || item.material.exists(_.isHerb) || item.brew.isDefined || item.gem.isDefined)

  /** Куда в доме вещь ляжет сейчас; `None` — не в дом: его нет, такого
    * хранилища в нём нет, оно полно или вещь туда не кладут. */
  def placeOf(home: HomeData, item: Item): Option[HomePlace] =
    home.house.filter(_ => wanted(item)).flatMap { house =>
      val bench = house.tier >= House.Townhouse.tier
      if (item.runeStone.isDefined) Some(HomePlace.Runes)
      else if (item.material.exists(_.isHerb))
        Option.when(bench && home.herbs.size < home.herbShelves)(HomePlace.Herbs)
      else if (item.brew.isDefined)
        Option.when(bench && home.brews.size < home.brewShelves)(HomePlace.Brews)
      else if (item.gem.isDefined)
        Option.when(house == House.Island && home.gems.size < home.gemShelves)(HomePlace.Gems)
      else None
    }

  private def put(home: HomeData, item: Item, place: HomePlace): HomeData = place match {
    case HomePlace.Runes =>
      item.runeStone.fold(home)(rs => home.withRune(rs.runeKey, home.runeCount(rs.runeKey) + rs.size.points))
    case HomePlace.Herbs => home.copy(herbs = home.herbs :+ item)
    case HomePlace.Brews => home.copy(brews = home.brews :+ item)
    case HomePlace.Gems  => home.copy(gems = home.gems :+ item)
  }
}
