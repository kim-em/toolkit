package net.tqft.toolkit.algebra.polynomials2

import scala.language.implicitConversions
import net.tqft.toolkit.algebra._

case class Polynomial[A](coefficients: Map[Int, A])

object Polynomial {
  implicit def polynomialAlgebraAsRing[A: EuclideanRing]: Ring[Polynomial[A]] = implicitly[PolynomialAlgebra[A, Polynomial[A]]]
  implicit def polynomialAlgebraOverFieldAsEuclideanRing[A: Field]: EuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverField[A, Polynomial[A]]]
  implicit def polynomialAlgebraOverOrderedFieldAsOrderedEuclideanRing[A: OrderedField]: OrderedEuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverOrderedField[A, Polynomial[A]]]
}


