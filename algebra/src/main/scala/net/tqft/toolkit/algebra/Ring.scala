package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.polynomials.Polynomial

trait Ring[A] extends Rig[A] with AdditiveCategory[Unit, A] with CommutativeGroup[A]

trait ImplicitRings {
  implicit def forget[A: EuclideanDomain]: Ring[A] = implicitly[EuclideanDomain[A]]
}

object Ring extends ImplicitRings