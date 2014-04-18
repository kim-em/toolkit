package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

import scala.language.implicitConversions

abstract class PolynomialsOverIntegerModel[A: IntegerModel] extends PolynomialsOverGCDRing[A] {
  override def ring: IntegerModel[A]
}

object PolynomialsOverIntegerModel {
  implicit def over[A: IntegerModel]: PolynomialsOverIntegerModel[A] = new PolynomialsOverIntegerModel[A] {
    override def ring = implicitly[IntegerModel[A]]
  }

  implicit def defaultFactorizationAlgorithm[A: IntegerModel](polynomials: PolynomialsOverIntegerModel[A]) = {
    import ZassenhausFactoring._
    polynomials: Factorization[Polynomial[A]]
  }
}
