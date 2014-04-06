//package net.tqft.toolkit.algebra.polynomials
//
//import net.tqft.toolkit.algebra._
//
//trait PolynomialAlgebraOverOrderedField[A, P] extends PolynomialAlgebraOverField[A, P] with PolynomialAlgebraOverOrderedEuclideanRing[A, P] with OrderedEuclideanRing[P] {
//  override def ring: OrderedField[A]
//}
//
//object PolynomialAlgebraOverOrderedField {
//  trait PolynomialAlgebraOverOrderedFieldForMaps[A] extends PolynomialAlgebraOverField.PolynomialAlgebraOverFieldForMaps[A] with PolynomialAlgebraOverOrderedField[A, Map[Int, A]] {
//    override def ring: OrderedField[A]
//  }
//  trait PolynomialAlgebraOverOrderedFieldForPolynomials[A] extends PolynomialAlgebraOverField.PolynomialAlgebraOverFieldForPolynomials[A] with PolynomialAlgebraOverOrderedField[A, Polynomial[A]] {
//    override def ring: OrderedField[A]
//  }
//
//  implicit def forMaps[A: OrderedField]: PolynomialAlgebraOverOrderedField[A, Map[Int, A]] = new PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverOrderedField[A, Map[Int, A]] {
//    override def ring = implicitly[OrderedField[A]]
//  }
//  implicit def over[A: OrderedField]: PolynomialAlgebraOverOrderedField[A, Polynomial[A]] = new PolynomialAlgebra.PolynomialAlgebraForPolynomials[A] with PolynomialAlgebraOverOrderedField[A, Polynomial[A]] {
//    override def ring = implicitly[OrderedField[A]]
//  }
//}
//
//trait PolynomialsOverOrderedField[A] extends PolynomialAlgebraOverOrderedField[A, Polynomial[A]]
//object PolynomialsOverOrderedField {
//  implicit def over[A: OrderedField]: PolynomialsOverOrderedField[A] = new PolynomialAlgebraOverOrderedField.PolynomialAlgebraOverOrderedFieldForPolynomials[A] with PolynomialsOverOrderedField[A] {
//    override def ring = implicitly[OrderedField[A]]
//  }
//}
//
