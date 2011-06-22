package net.tqft.toolkit

object NonStrictNaturalNumbers extends NonStrictIterable[Int] {
	def iterator = NonStrictIterable.iterate(0)(_ + 1).iterator
}