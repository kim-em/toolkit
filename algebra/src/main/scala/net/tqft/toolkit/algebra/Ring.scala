package net.tqft.toolkit.algebra

trait Ring[@specialized(Int, Long, Float, Double) A] extends Rig[A] with AdditiveGroup[A]

trait CommutativeRing[A] extends CommutativeRig[A] with Ring[A]

trait RingLowPriorityImplicits {
  implicit def pointwiseRingMap[A, B: Ring]: Ring[Map[A, B]] = new Ring.PointwiseRingMap[A, B]
}

object Ring extends RingLowPriorityImplicits {
  implicit def forget[A: EuclideanRing]: Ring[A] = implicitly[EuclideanRing[A]]

  protected trait RingMapLike[A, B] extends Ring[Map[A, B]] with Module[B, Map[A, B]] {
    def values: Ring[B]

    override def negate(m: Map[A, B]) = m.mapValues(values.negate)
  }

  class RingMap[A: AdditiveMonoid, B: Ring] extends Rig.RigMap[A, B] with RingMapLike[A, B] {
    override def values = implicitly[Ring[B]]
  }

  class PointwiseRingMap[A, B: Ring] extends Rig.PointwiseRigMap[A, B] with RingMapLike[A, B] {
    override def values = implicitly[Ring[B]]
  }

  class RingSeq[B: Ring] extends Rig.RigSeq[B] with Ring[Seq[B]] with Module[B, Seq[B]] {
    override def values = implicitly[Ring[B]]

    override def scalarMultiply(b: B, s: Seq[B]) = s.map(v => values.multiply(b, v))
    override def negate(s: Seq[B]) = s.map(values.negate)
  }

  implicit def ringMap[A: AdditiveMonoid, B: Ring]: Ring[Map[A, B]] = new RingMap[A, B]
  implicit def ringSeq[B: Ring]: Ring[Seq[B]] = new RingSeq[B]

}

