package net.tqft.toolkit.algebra

import scala.collection.GenTraversableOnce

object AlgebraicNotation {
  implicit class AdditiveMonoidElement[A: AdditiveMonoid](a: A) {
    def +(b: A) = implicitly[AdditiveMonoid[A]].add(a, b)
  }
  implicit class AdditiveGroupElement[A: AdditiveGroup](a: A) {
    private def group = implicitly[AdditiveGroup[A]]
    def -(b: A) = group.subtract(a, b)
    def unary_-() = group.negate(a)
  }

  implicit class RigElement[A: Rig](a: A) {
    private def rig = implicitly[Rig[A]]

    def *(b: A) = rig.multiply(a, b)
    def ^(k: Int) = rig.power(a, k)
  }

  implicit class IntAsRigElement[A: Rig](i: Int) {
    def *(a: A) = implicitly[Rig[A]].multiplyByInt(a, i)
  }
  
  implicit class EuclideanRigElement[A: EuclideanRig](a: A) {
    private def domain = implicitly[EuclideanRig[A]]
    def %(b: A) = domain.remainder(a, b)
    def /(b: A) = domain.quotient(a, b)
  }

  implicit class ModuleElement[A, B](b: B)(implicit module: Module[A, B]) {
    def *:(a: A) = module.scalarMultiply(a, b)
  }

}