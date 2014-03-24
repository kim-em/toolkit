package net.tqft.toolkit.algebra

trait Algebra[A, B] extends Ring[B] with Module[A, B]

object Algebra {
//  implicit def fromRing[R](implicit ring: Ring[R]): Algebra[R, R] = {
//    new Algebra[R, R] {
//    	override def zero = ring.zero
//    	override def one = ring.one
//    	override def fromInt(k: Int) = ring.fromInt(k)
//    	override def negate(r: R) = ring.negate(r)
//    	override def add(a: R, b: R) = ring.add(a, b)
//    	override def multiply(a: R, b: R) = ring.multiply(a, b)
//    	override def scalarMultiply(a: R, b: R) = ring.multiply(a, b)
//    }
//  }
//  implicit def forgetPolynomialAlgebra[A: polynomials.PolynomialAlgebra]: Algebra[A, polynomials.Polynomial[A]] = implicitly[Algebra[A, polynomials.Polynomial[A]]]
}

trait AssociativeAlgebra[A, B] extends Algebra[A, B]