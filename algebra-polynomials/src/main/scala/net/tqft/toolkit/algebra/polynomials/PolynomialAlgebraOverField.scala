//package net.tqft.toolkit.algebra.polynomials
//
//import net.tqft.toolkit.algebra._
//
//trait PolynomialAlgebraOverField[A, P] extends PolynomialAlgebraOverEuclideanRing[A, P] {
//  override def ring: Field[A]
//}
//
//object PolynomialAlgebraOverField {
//  trait PolynomialAlgebraOverFieldForMaps[A] extends PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverField[A, Map[Int, A]] {
//    override def ring: Field[A]
//  }
//  trait PolynomialAlgebraOverFieldForPolynomials[A] extends PolynomialAlgebra.PolynomialAlgebraForPolynomials[A] with PolynomialAlgebraOverField[A, Polynomial[A]] {
//    override def ring: Field[A]
//  }
//
//  implicit def forMaps[A: Field]: PolynomialAlgebraOverField[A, Map[Int, A]] = new PolynomialAlgebraOverFieldForMaps[A] {
//    override def ring = implicitly[Field[A]]
//  }
//  implicit def over[A: Field]: PolynomialAlgebraOverField[A, Polynomial[A]] = new PolynomialAlgebra.PolynomialAlgebraForPolynomials[A] with PolynomialAlgebraOverField[A, Polynomial[A]] {
//    override def ring = implicitly[Field[A]]
//  }
//}
//
//trait PolynomialsOverField[A] extends PolynomialAlgebraOverField[A, Polynomial[A]] with Polynomials[A]
//object PolynomialsOverField {
//  implicit def over[A: Field]: PolynomialsOverField[A] = new PolynomialAlgebraOverField.PolynomialAlgebraOverFieldForPolynomials[A] with PolynomialsOverField[A] {
//    override def ring = implicitly[Field[A]]
//  }
//}
//
