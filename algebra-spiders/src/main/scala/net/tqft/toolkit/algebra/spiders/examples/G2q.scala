package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra._


object `(G_2)_q` extends PentagonReductionSpider[RationalFunction[BigInt]] {
  override val ring = implicitly[Field[RationalFunction[BigInt]]]

  override val d: RationalFunction[BigInt] = Fraction(Map(10 -> 1, 9 -> 1, 6 -> 1, 5 -> 1, 4 -> 1, 1 -> 1, 0 -> 1), Map(5 -> 1))
  override val b: RationalFunction[BigInt] = Fraction(Map(6 -> 1, 5 -> 1, 4 -> 1, 2 -> 1, 1 -> 1, 0 -> 1), Map(3 -> 1))
  override val t: RationalFunction[BigInt] = Fraction(Map(4 -> -1, 2 -> -1, 0 -> -1), Map(2 -> 1))
  override val omega = ring.one
}
