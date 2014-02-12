package net.tqft.toolkit.algebra.categories

import net.tqft.toolkit.algebra._

trait LinearCategory[O, M, R] extends AdditiveCategory[O, M] { lc =>
  def scalarMultiply(r: R, m: M): M

  protected class EndomorphismAlgebra(o: O) extends EndomorphismRing(o) with Algebra[R, M] {
    override def scalarMultiply(a: R, b: M) = lc.scalarMultiply(a, b)
  }

  override def endomorphisms(o: O): Algebra[R, M] = new EndomorphismAlgebra(o)
}

object LinearCategory {
  implicit def ringAsLinearCategory[A](ring: Ring[A]): LinearCategory[Unit, A, A] = {
    new LinearCategory[Unit, A, A] {
      override def identityMorphism(o: Unit) = ring.one
      override def source(a: A) = ()
      override def target(a: A) = ()
      override def compose(x: A, y: A) = ring.multiply(x, y)
      override def zeroMorphism(o1: Unit, o2: Unit): A = ring.zero
      override def negate(a: A) = ring.negate(a)
      override def add(x: A, y: A) = ring.add(x, y)
      override def scalarMultiply(x: A, y: A) = ring.multiply(x, y)
    }
  }  
}
