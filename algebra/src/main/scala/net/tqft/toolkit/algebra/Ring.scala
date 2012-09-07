package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.categories._
import net.tqft.toolkit.algebra.polynomials.Polynomial

trait Ring[@specialized(Int, Long, Float, Double) A] extends Rig[A] with AdditiveGroup[A]

trait CommutativeRing[A] extends CommutativeRig[A] with Ring[A]

object Ring {
  implicit def forget[A: EuclideanRing]: Ring[A] = implicitly[EuclideanRing[A]]
}

trait RingHomomorphism[A, B] extends Homomorphism[Ring, A, B]

object Rings extends HomomorphismCategory[Ring]
