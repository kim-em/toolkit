package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

abstract class PolynomialsOverIntegerModel[A: IntegerModel] extends PolynomialsOverGCDRing[A] {
  override def ring: IntegerModel[A]
}

object PolynomialsOverIntegerModel {
  implicit def over[A:IntegerModel]: PolynomialsOverIntegerModel[A] = new PolynomialsOverIntegerModel[A] {
    override def ring = implicitly[IntegerModel[A]]
  }

  implicit def defaultFactorizationAlgorithm[A: IntegerModel:Factorization](polynomials: PolynomialsOverIntegerModel[A]) = {
    import ZassenhausFactoring._
    polynomials: Factorization[Polynomial[A]]
  }
}
