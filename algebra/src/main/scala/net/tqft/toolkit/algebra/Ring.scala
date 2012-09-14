package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.categories._
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebra
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial

trait Ring[@specialized(Int, Long, Float, Double) A] extends Rig[A] with AdditiveGroup[A]

trait CommutativeRing[A] extends CommutativeRig[A] with Ring[A]

object Ring {
  implicit def forget[A: EuclideanRing]: Ring[A] = implicitly[EuclideanRing[A]]
  implicit def forgetMultivariablePolynomialAlgebra[A, V](implicit algebra: MultivariablePolynomialAlgebra[A, V]): Ring[MultivariablePolynomial[A, V]] = algebra
}

trait RingHomomorphism[A, B] extends Homomorphism[Ring, A, B]

object Rings extends HomomorphismCategory[Ring]
