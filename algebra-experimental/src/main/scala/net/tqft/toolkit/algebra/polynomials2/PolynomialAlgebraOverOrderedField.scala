package net.tqft.toolkit.algebra.polynomials2

import net.tqft.toolkit.algebra._

trait PolynomialAlgebraOverOrderedField[A, P] extends PolynomialAlgebraOverField[A, P] with OrderedEuclideanRing[P] {
  override def ring: OrderedField[A]
  
  override def compare(p1: P, p2: P) = ???
}

object PolynomialAlgebraOverOrderedField {
  implicit def forMaps[A: OrderedField]: PolynomialAlgebraOverOrderedField[A, Map[Int, A]] = new PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverOrderedField[A, Map[Int, A]] {
    override def ring = implicitly[OrderedField[A]]
  }
  implicit def over[A: OrderedField]: PolynomialAlgebraOverOrderedField[A, Polynomial[A]] = new PolynomialAlgebra.PolynomialAlgebraForPolynomialWrapper[A]with PolynomialAlgebraOverOrderedField[A, Polynomial[A]] {
    override def ring = implicitly[OrderedField[A]]
  }
}
