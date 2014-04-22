package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.polynomials._

object `SO(3)_q` extends PentagonReductionSpider[RationalFunction[Int]] {
  val q: RationalFunction[Int] = Polynomial(1 -> 1)
  override val d = ring.add(q, ring.inverse(q))
  override val b = ???
  override val t = ???
  override val omega = ring.one
}
