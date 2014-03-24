package net.tqft.toolkit.algebra

trait Ring[@specialized(Int, Long, Float, Double) A] extends Rig[A] with AdditiveGroup[A]

trait CommutativeRing[A] extends CommutativeRig[A] with Ring[A]

object Ring {
  implicit def forget[A: EuclideanRing]: Ring[A] = implicitly[EuclideanRing[A]]
}

