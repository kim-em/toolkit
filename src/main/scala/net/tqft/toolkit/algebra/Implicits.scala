package net.tqft.toolkit.algebra

object Implicits {
  implicit val Integers = Gadgets.Integers
  implicit val Rationals = Gadgets.Rationals

  implicit val BigIntegers = Gadgets.BigIntegers
  implicit val BigRationals = Gadgets.BigRationals
  implicit val BigDecimals = Gadgets.BigDecimals
  
  implicit val integersAsRationals = Gadgets.integersAsRationals
  implicit val bigIntegersAsBigRationals = Gadgets.bigIntegersAsBigRationals
  implicit val integersAsBigInts = Gadgets.integersAsBigInts

  implicit val IntegerPolynomials = Gadgets.IntegerPolynomials
  implicit val RationalPolynomials = Gadgets.RationalPolynomials
  implicit val BigIntegerPolynomials = Gadgets.BigIntegerPolynomials
  implicit val BigRationalPolynomials = Gadgets.BigRationalPolynomials

  implicit def asConstantPolynomial[A](a: A)(implicit ring: Ring[A]): Polynomial[A] = Polynomial[A]((0, a))
}