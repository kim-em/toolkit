package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.numberfields._
import net.tqft.toolkit.algebra._

abstract class PolynomialsOverFiniteField[I: FiniteField] extends PolynomialsOverField[Polynomial[I]] {
  override def ring: FiniteField[I] = implicitly[FiniteField[I]]
  def irreducible_?(p: Polynomial[I]): Boolean = {
    val modQ = PrimeField(ring.characteristic)(ring.integers)
    val quotientRing: Ring[Polynomial[I]] = PolynomialQuotientRing(p)(modQ)
    val polynomials = Polynomials.over(modQ)
    def r(k: Int) = {
      subtract(quotientRing.power(polynomials.monomial(1), ring.integers.power(ring.order, k))(ring.integers), polynomials.monomial(1))
    }
    val n = maximumDegree(p).get
    (for (d <- Integers.divisors(n); if d != n; if euclideanAlgorithm(p, r(d)) == one) yield d).isEmpty && exactQuotientOption(r(n), p).nonEmpty
  }
}

object PolynomialsOverFiniteField {
  def over[I: FiniteField]: PolynomialsOverFiniteField[I] = new PolynomialsOverFiniteField[I] {
    override def ring = implicitly[FiniteField[I]]
  }
}
