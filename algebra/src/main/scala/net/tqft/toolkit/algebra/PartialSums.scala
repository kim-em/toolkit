package net.tqft.toolkit.algebra

object PartialSums {
  implicit class PartialSummable[A: AdditiveMonoid](xs: Seq[A]) {
    private def monoid = implicitly[AdditiveMonoid[A]]
    def partialSums = xs.foldLeft(List(monoid.zero))((list, x) => monoid.add(list.head, x) :: list).reverse
  }
}