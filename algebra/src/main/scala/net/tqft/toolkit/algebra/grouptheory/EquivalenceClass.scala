package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.algebra._

trait EquivalenceClass[A] extends Elements[A] {
  def representative: A
  
  def contains(a: A) = elements.contains(a)
//  final def contains(a: Any) = a match { // type parameter here ought to be a: A, but I ran into <https://issues.scala-lang.org/browse/SI-6522>
//    case a: A => typedContains(a)
//    case _ => false
//  }
//  def typedContains(a: A) = elements.contains(a)

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

