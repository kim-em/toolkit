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

trait ImplicitRigs {
  implicit val CountableCardinals: Rig[CountableCardinal] = CountableCardinals
  implicit val Integers: Rig[Int] = Gadgets.Integers
  implicit val Rationals: Rig[Fraction[Int]] = Gadgets.Rationals
  implicit val BigInts: Rig[BigInt] = Gadgets.BigIntegers
  implicit val BigRationals: Rig[Fraction[BigInt]] = Gadgets.BigRationals  
  implicit val Doubles: Rig[Double] = Gadgets.Doubles
  implicit val RationalPolynomials: Rig[Polynomial[Fraction[Int]]] = Gadgets.RationalPolynomials
}

object Rig extends ImplicitRigs