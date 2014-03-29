package net.tqft.toolkit.algebra.polynomials2

import scala.language.implicitConversions
import net.tqft.toolkit.algebra._

case class PowerSeries[A](coefficients: Stream[A])

object PowerSeries {
  // TODO
  //  implicit def polynomialAlgebraAsRing[A: EuclideanRing]: Ring[PowerSeries[A]] = implicitly[PolynomialAlgebra[A, PowerSeries[A]]]
}


