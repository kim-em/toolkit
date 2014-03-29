package net.tqft.toolkit.algebra

trait Ring[@specialized(Int, Long, Float, Double) A] extends Rig[A] with AdditiveGroup[A]

trait CommutativeRing[A] extends CommutativeRig[A] with Ring[A]

object Ring {
  implicit def forget[A: EuclideanRing]: Ring[A] = implicitly[EuclideanRing[A]]

  class RingMap[A: AdditiveMonoid, B: Ring] extends Rig.RigMap[A, B] with Ring[Map[A, B]] with Module[B, Map[A, B]] {
    override def values = implicitly[Ring[B]]

    override def scalarMultiply(b: B, m: Map[A, B]) = m.mapValues(v => values.multiply(b, v))
    override def negate(m: Map[A, B]) = m.mapValues(values.negate)
  }
  
  class RingSeq[B: Ring] extends Rig.RigSeq[B] with Ring[Seq[B]] with Module[B, Seq[B]] {
    override def values = implicitly[Ring[B]]

    override def scalarMultiply(b: B, s: Seq[B]) = s.map(v => values.multiply(b, v))
    override def negate(s: Seq[B]) = s.map(values.negate)
  }

  implicit def ringMap[A: AdditiveMonoid, B: Ring]: Ring[Map[A, B]] = new RingMap[A, B]
  implicit def ringSeq[B: Ring]: Ring[Seq[B]] = new RingSeq[B]

}

