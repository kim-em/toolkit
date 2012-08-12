package net.tqft.toolkit.algebra

trait Rig[A] extends NLinearCategory[Unit, A] with Monoid[A] with CommutativeMonoid[A] {
  override def identityMorphism(o: Unit) = one
  override def source(a: A) = ()
  override def target(a: A) = ()
  override def compose(x: A, y: A) = multiply(x, y)
  override def zeroMorphism(o1: Unit, o2: Unit): A = zero

  def multiplyByInt(x: A, y: Int): A = multiply(x, fromInt(y))

  def fromInt(x: Int): A
}
