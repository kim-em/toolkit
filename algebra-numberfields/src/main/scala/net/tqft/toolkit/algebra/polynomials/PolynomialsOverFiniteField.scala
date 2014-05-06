package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.numberfields._
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.IntegerModel.defaultFactorizationAlgorithm_

abstract class PolynomialsOverFiniteField[I: FiniteField] extends PolynomialsOverField[Polynomial[I]] {
  override def ring: FiniteField[I] = implicitly[FiniteField[I]]
  def irreducible_?(p: Polynomial[I]): Boolean = {
    val modQ = PrimeField(ring.characteristic)(ring.integers)
    val quotientRing: Ring[Polynomial[I]] = PolynomialQuotientRing(p)(modQ)
    val polynomials = PolynomialsOverField.over(modQ)
    def r(k: Int) = {
      val `q^k` = ring.integers.power(ring.order, k)
      val `x^(q^k)` = quotientRing.power(polynomials.monomial(1), `q^k`)(ring.integers)
      val x = polynomials.monomial(1)
      val result = polynomials.subtract(`x^(q^k)`, x)
      require(result.toMap.values.forall(ring.integers.compare(_ ,ring.integers.zero) > 0))
      result
    }
    val n = maximumDegree(p).get
    (for (d <- Integers.divisors(n); if d != n; if polynomials.euclideanAlgorithm(p, r(d)) == one) yield d).isEmpty && polynomials.exactQuotientOption(r(n), p).nonEmpty
  }
  def randomPolynomial(degree: Int): Polynomial[Polynomial[I]] = {
    ???
  }
  
  override def toString = s"PolynomialsOverFiniteField.over($ring)"
}

object PolynomialsOverFiniteField {
  def over[I: FiniteField]: PolynomialsOverFiniteField[I] = new PolynomialsOverFiniteField[I] {
    override def ring = implicitly[FiniteField[I]]
  }
}
