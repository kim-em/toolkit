package net.tqft.toolkit.collections

object NonStrictNaturalNumbers extends NonStrictIterable[Int] {
	def iterator = NonStrictIterable.iterate(0)(_ + 1).iterator
}