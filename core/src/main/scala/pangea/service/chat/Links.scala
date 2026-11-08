package pangea.service.chat

/** Ссылки, которые бот даёт игрокам: статья-туториал о том, как всё устроено
  * в игре, приглашения в общую и торговую беседы и страница игрока в ВК.
  * Одним местом: тексты берут их подстановкой (`{tutorial}`, `{chat}`,
  * `{tradeChat}`). */
object Links {

  /** Статья «Впервые прибыли на Пангею? Обязательно к прочтению». */
  val Tutorial: String = "https://vk.ru/@stigmapangeaproject-vpervye-pribyli-na-pangeu-obyazatelno-k-prochteniu"

  /** Приглашение в общую беседу Пангеи. */
  val Chat: String = "https://vk.me/join/D_KzFZY1BWBtxF1fRuoZrheMRN5TB_dlWug="

  /** Приглашение в торговую беседу: туда аукцион пишет о лотах и передачах. */
  val TradeChat: String = "https://vk.me/join/3KSleJCxaRJ1hEQEZNW452t/S007y2OayuY="

  /** Страница игрока в ВК — по ней в топе Mini App открывают того, кто в нём стоит. */
  def profile(vkId: String): String = s"https://vk.ru/id$vkId"
}
