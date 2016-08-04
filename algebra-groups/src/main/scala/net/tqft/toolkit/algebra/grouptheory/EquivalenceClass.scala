package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.algebra._

trait EquivalenceClass[A] extends Finite[A] {
  def representative: A
  override def elements: Set[A]
  
  def contains(a: A) = elements.contains(a)

  def leastRepresentative(implicit o: Ordering[A]) = elements.min

  override def equals(other: Any) = {
    other match {
      case other: EquivalenceClass[_] => {
        contains(other.asInstanceOf[EquivalenceClass[A]].representative)
      }
      case _ => false
    }
  }
  override def hashCode = {
    elements.hashCode
  }
}

object EquivalenceClass {
  implicit def equivalenceClassOrdering[A](implicit o: Ordering[A]): Ordering[EquivalenceClass[A]] = new Ordering[EquivalenceClass[A]] {
    def compare(x: EquivalenceClass[A], y: EquivalenceClass[A]) = o.compare(x.leastRepresentative, y.leastRepresentative)
  }
}

trait Orbit[A, B] extends EquivalenceClass[B] {
  def stabilizer: FiniteGroup[A]
}

object Orbit {
  def singleton[A, B](group: FiniteGroup[A], b: B) = new Orbit[A, B] {
    override def representative = b
    override def elements = Set(b)
    override def stabilizer= group
  }
}

