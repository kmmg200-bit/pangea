package pangea.model.payment

/** Суммы доната — других в прайсе нет. У каждой своя история Ришелье, а кто
  * хоть раз купил каждую из семи — «Любимец Проекта». */
object DonationTiers {

  /** Суммы в копейках: 200, 500, 1000, 2000, 4000, 6000 и 10 000 ₽. */
  val Amounts: List[Long] = List(20000L, 50000L, 100000L, 200000L, 400000L, 600000L, 1000000L)

  /** Куплена ли каждая сумма хотя бы раз. `granted` — суммы выданных донатов. */
  def complete(granted: List[Long]): Boolean = Amounts.forall(granted.contains)
}
