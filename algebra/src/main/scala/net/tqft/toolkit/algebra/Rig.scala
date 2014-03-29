package net.tqft.toolkit.algebra

trait Rig[@specialized(Int, Long, Float, Double) A] extends Monoid[A] with AdditiveMonoid[A] {
  def multiplyByInt(x: A, y: Int): A = multiply(x, fromInt(y))
  def fromInt(x: Int): A = {
    require(x >= 0)
    if (x == 0) {
      zero
    } else {
      Iterator.fill(x)(one).reduce(add)
    }
  }
}

trait RigLowPriorityImplicits {
  implicit def pointwiseRigMap[A, B: Rig]: Rig[Map[A, B]] = new Rig.PointwiseRigMap[A, B] {
    override def coefficients = implicitly[ModuleOverRig[B, B]]
    override def multiplicativeCoefficients = implicitly[Rig[B]]
  }
}

object Rig extends RigLowPriorityImplicits {
  implicit def forget[A: Ring]: Rig[A] = implicitly[Ring[A]]

  trait RigMap[A, B] extends ModuleOverRig.ModuleOverRigMap[B, A, B] with Rig[Map[A, B]] {
    def keys: AdditiveMonoid[A]
    def multiplicativeCoefficients: Rig[B]

    override def one = Map(keys.zero -> multiplicativeCoefficients.one)
    override def multiply(m1: Map[A, B], m2: Map[A, B]): Map[A, B] = {
      val newMap = scala.collection.mutable.Map[A, B]().withDefault(_ => coefficients.zero)
      for ((a1, b1) <- m1; (a2, b2) <- m2) {
        val p = keys.add(a1, a2)
        newMap(p) = coefficients.add(newMap(p), multiplicativeCoefficients.multiply(b1, b2))
      }
      Map() ++ newMap
    }
  }

  trait PointwiseRigMap[A, B] extends ModuleOverRig.ModuleOverRigMap[B, A, B] with Rig[Map[A, B]] {
    def multiplicativeCoefficients: Rig[B]

    override def one = Map().withDefault(_ => multiplicativeCoefficients.one)
    override def multiply(m1: Map[A, B], m2: Map[A, B]): Map[A, B] = {
      for ((a, b) <- m1; c = m2.get(a).getOrElse(coefficients.zero)) yield (a -> multiplicativeCoefficients.multiply(b, c))
    }
  }

  class RigSeq[B: Rig] extends AdditiveMonoid.AdditiveMonoidSeq[B] with Rig[Seq[B]] {
    override def coefficients = implicitly[Rig[B]]

    override def one = Seq(coefficients.one)
    override def multiply(s1: Seq[B], s2: Seq[B]): Seq[B] = {
      ???
    }
  }

  implicit def rigMap[A: AdditiveMonoid, B: Rig]: Rig[Map[A, B]] = new RigMap[A, B] {
    override def keys = implicitly[AdditiveMonoid[A]]
    override def coefficients = implicitly[ModuleOverRig[B, B]]
    override def multiplicativeCoefficients = implicitly[Rig[B]]
  }
  implicit def rigSeq[B: Rig]: Rig[Seq[B]] = new RigSeq[B]

}

trait CommutativeRig[A] extends Rig[A]