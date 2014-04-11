package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

abstract class PolynomialsOverFieldOfFractions[A: GCDRing] extends PolynomialsOverEuclideanRing[Fraction[A]] {
  def primitivePartOverFractions(q: Polynomial[Fraction[A]]): Polynomial[A] = {
    val gcdRing = implicitly[GCDRing[A]]

    val c = gcdRing.product(q.coefficients.values.map(_.denominator))
    val p = scalarMultiply(c, q).coefficients.mapValues(_.numerator)
    val g = gcdRing.gcd(p.values.toSeq: _*)
    p.mapValues(x => gcdRing.exactQuotient(x, g))
  }
}
object PolynomialsOverFieldOfFractions {
  implicit def over[A: GCDRing]: PolynomialsOverFieldOfFractions[A] = new PolynomialsOverFieldOfFractions[A] {
    override def ring = implicitly[EuclideanRing[Fraction[A]]]
  }
}

