package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.polynomials._

object `SO(3)_q` extends PentagonReductionSpider[MultivariableRationalFunction[BigInt, String]] {
  val q: MultivariableRationalFunction[BigInt, String] = "q": MultivariablePolynomial[BigInt, String]
  override val d = ring.add(ring.power(q, 2), ring.one, ring.power(q, -2))
  override val b = ring.one
  override val t = ring.quotient(ring.add(ring.power(q, 2), ring.negativeOne, ring.power(q, -2)), ring.add(ring.power(q, 2), ring.power(q, -2)))
  override val omega = ring.one
}
