package net.tqft.toolkit.algebra

trait Ring[@specialized(Int, Long, Float, Double) A] extends Rig[A] with AdditiveGroup[A]

trait CommutativeRing[A] extends CommutativeRig[A] with Ring[A]

trait RingLowPriorityImplicits {
  implicit def pointwiseRingMap[A, B: Ring]: Ring[Map[A, B]] = new Ring.PointwiseRingMap[A, B]
}

object Ring extends RingLowPriorityImplicits {
  implicit def forget[A: EuclideanRing]: Ring[A] = implicitly[EuclideanRing[A]]

  trait RingMap[A, B] extends Module.ModuleMap[B, A, B] with Rig.RigMap[A, B]  with Ring[Map[A, B]] {
    override def coefficients: Module[B, B]
  }

  class PointwiseRingMap[A, B: Ring] extends Rig.PointwiseRigMap[A, B] with Module.ModuleMap[B, A, B] with Ring[Map[A, B]] {
    override def coefficients = Module.moduleOverItself(multiplicativeCoefficients)
    override def multiplicativeCoefficients = implicitly[Ring[B]]
  }

  class RingSeq[B: Ring] extends Rig.RigSeq[B] with Ring[Seq[B]] with Module[B, Seq[B]] {
    override def coefficients = implicitly[Ring[B]]

    override def scalarMultiply(b: B, s: Seq[B]) = s.map(v => coefficients.multiply(b, v))
    override def negate(s: Seq[B]) = s.map(coefficients.negate)
  }

  implicit def ringMap[A: AdditiveMonoid, B: Ring]: Ring[Map[A, B]] = new RingMap[A, B] {
    override def keys = implicitly[AdditiveMonoid[A]]
    override def multiplicativeCoefficients = implicitly[Ring[B]]
    override def coefficients = Module.moduleOverItself(multiplicativeCoefficients)
  }
  implicit def ringSeq[B: Ring]: Ring[Seq[B]] = new RingSeq[B]

}

