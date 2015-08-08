package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

abstract class PolynomialsOverFieldOfFractions[A: GCDRing] extends PolynomialsOverField[Fraction[A]] {
  def primitivePartOverFractions(q: Polynomial[Fraction[A]]): Polynomial[A] = {
    val gcdRing = implicitly[GCDRing[A]]

    val c = gcdRing.product(toMap(q).values.map(_.denominator))
    val p = scalarMultiply(c, q).mapValues(_.numerator)
    val g = gcdRing.gcd(p.toMap.values.toSeq: _*)
    p.mapValues(x => gcdRing.exactQuotient(x, g))
  }
}
object PolynomialsOverFieldOfFractions {
  implicit def over[A: GCDRing]: PolynomialsOverFieldOfFractions[A] = new PolynomialsOverFieldOfFractions[A] {
    override def ring = implicitly[Field[Fraction[A]]]
  }
}

