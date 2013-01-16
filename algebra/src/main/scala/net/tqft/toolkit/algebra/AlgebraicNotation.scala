package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.modules.Module

object AlgebraicNotation {
  implicit def additiveMonoidElement[A: AdditiveMonoid](a: A) = new AdditiveMonoidElement(a)
  class AdditiveMonoidElement[A: AdditiveMonoid](a: A) {
    def +(b: A) = implicitly[AdditiveMonoid[A]].add(a, b)
  }
  implicit def additiveGroupElement[A: AdditiveGroup](a: A) = new AdditiveGroupElement(a)
  class AdditiveGroupElement[A: AdditiveGroup](a: A) {
    private def group = implicitly[AdditiveGroup[A]]
    def -(b: A) = group.subtract(a, b)
    def unary_-() = group.negate(a)
  }

  implicit def rigElement[A: Rig](a: A) = new RigElement(a)
  class RigElement[A: Rig](a: A) {
    private def rig = implicitly[Rig[A]]

    def *(b: A) = rig.multiply(a, b)
    def ^(k: Int) = rig.power(a, k)
  }

  implicit def euclideanRigElement[A: EuclideanRig](a: A) = new EuclideanRigElement(a)
  class EuclideanRigElement[A: EuclideanRig](a: A) {
    private def domain = implicitly[EuclideanRig[A]]
    def %(b: A) = domain.remainder(a, b)
    def /(b: A) = domain.quotient(a, b)
  }

  implicit def moduleElement[A, B](b: B)(implicit module: Module[A, B]) = new ModuleElement(b)
  class ModuleElement[A, B](b: B)(implicit module: Module[A, B]) {
    def *:(a: A) = module.scalarMultiply(a, b)
  }

}