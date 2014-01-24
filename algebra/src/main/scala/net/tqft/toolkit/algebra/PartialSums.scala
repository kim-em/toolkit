package net.tqft.toolkit.algebra

object PartialSums {
	implicit def toPartialSummable[A:AdditiveMonoid](xs: Seq[A]): PartialSummable[A] = new PartialSummable(xs)
	class PartialSummable[A:AdditiveMonoid](xs: Seq[A]) {
	  val monoid = implicitly[AdditiveMonoid[A]]
	  def partialSums = xs.foldLeft(List(monoid.zero))((list, x) => monoid.add(list.head, x) :: list).reverse
	}
}