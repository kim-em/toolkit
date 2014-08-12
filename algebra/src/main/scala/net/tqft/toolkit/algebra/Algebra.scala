package net.tqft.toolkit.algebra

trait Algebra[A, B] extends Ring[B] with Module[A, B]

object Algebra {
  def fromRing[B](ring: Ring[B]) = new Algebra[B, B] {
    override def add(x: B, y: B) = ring.add(x, y)
    override def multiply(x: B, y: B) = ring.multiply(x, y)
    override def negate(x: B) = ring.negate(x)
    override def scalarMultiply(x: B, y: B) = multiply(x, y)
    override def zero = ring.zero
    override def one = ring.one
  }
}

trait AssociativeAlgebra[A, B] extends Algebra[A, B]