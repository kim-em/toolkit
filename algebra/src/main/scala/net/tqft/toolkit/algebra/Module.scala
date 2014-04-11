package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra._

trait ModuleOverRig[A, B] extends AdditiveMonoid[B] {
  def scalarMultiply(a: A, b: B): B
}

object ModuleOverRig {
  implicit def forget[A, B](implicit module: Module[A, B]): ModuleOverRig[A, B] = module

  protected[algebra] class ModuleOverItself[B: Rig] extends ModuleOverRig[B, B] {
    private val rig = implicitly[Rig[B]]
    override def zero = rig.zero
    override def add(b1: B, b2: B) = rig.add(b1, b2)
    override def scalarMultiply(b1: B, b2: B) = rig.multiply(b1, b2)
  }

  implicit def moduleOverItself[B: Rig]: ModuleOverRig[B, B] = new ModuleOverItself[B]

  trait ModuleOverRigMap[A, B, C] extends AdditiveMonoid.AdditiveMonoidMap[B, C] with ModuleOverRig[A, Map[B, C]] {
    override def coefficients: ModuleOverRig[A, C]

    override def scalarMultiply(a: A, m: Map[B, C]) = m.mapValues(c => coefficients.scalarMultiply(a, c)).filter({ case (_, v) => v != coefficients.zero })
  }

  implicit def moduleOverRigMap[A, B, C](implicit module: ModuleOverRig[A, C]): ModuleOverRig[A, Map[B, C]] = new ModuleOverRigMap[A, B, C] {
    override def coefficients = module
  }

}

trait Module[A, B] extends ModuleOverRig[A, B] with AdditiveGroup[B]

object Module {
  implicit def forget[A, B](implicit vectorSpace: VectorSpace[A, B]): Module[A, B] = vectorSpace

  protected class ModuleOverItself[B: Ring] extends ModuleOverRig.ModuleOverItself[B] with Module[B, B] {
    private val ring = implicitly[Ring[B]]
    override def negate(b: B) = ring.negate(b)
  }

  implicit def moduleOverItself[B: Ring]: Module[B, B] = new ModuleOverItself[B]

  trait ModuleMap[A, B, C] extends ModuleOverRig.ModuleOverRigMap[A, B, C] with Module[A, Map[B, C]] {
    override def coefficients: Module[A, C]

    override def negate(m: Map[B, C]) = m.mapValues(c => coefficients.negate(c))
  }
  implicit def moduleMap[A, B, C](implicit module: Module[A, C]): Module[A, Map[B, C]] = new ModuleMap[A, B, C] {
    override def coefficients = module
  }

}

