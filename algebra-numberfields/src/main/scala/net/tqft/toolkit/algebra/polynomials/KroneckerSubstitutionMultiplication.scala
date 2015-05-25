package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

trait KroneckerSubstitutionMultiplication[A] { self: PolynomialsOverIntegerModel[A] =>
  override def multiply(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {
    if (ring.isInstanceOf[ArbitraryPrecisionIntegerModel[A]]) {
      implicit def ring_ : Ring[A] = ring
      val t: Int = x.toMap.values.map(v => ring.log(v, 2)).max + y.toMap.values.map(v => ring.log(v, 2)).max + Integers.log(Seq(maximumDegree(x).getOrElse(0), maximumDegree(y).getOrElse(0)).max, 2)
      val z = ring.power(ring.fromInt(2), t)
      ring.digits(ring.multiply(evaluateAt(z)(x), evaluateAt(z)(y)), z)
    } else {
      implicit def ring = self.ring
      implicitly[PolynomialsOverIntegerModel[BigInt]].multiply(x.mapValues(v => ring.toBigInt(v)), y.mapValues(v => ring.toBigInt(v))).mapValues(v => ring.fromBigInt(v))
    }
  }
}

object KroneckerSubstitutionMultiplication {
  def over[A: IntegerModel]: PolynomialsOverIntegerModel[A] = new PolynomialsOverIntegerModel[A] with KroneckerSubstitutionMultiplication[A] {
    override def ring = implicitly[IntegerModel[A]]
  }
}

