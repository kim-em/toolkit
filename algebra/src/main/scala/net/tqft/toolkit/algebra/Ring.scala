package net.tqft.toolkit.algebra

trait Ring[A] extends Rig[A] with AdditiveCategory[Unit, A] with CommutativeGroup[A]

trait ImplicitRings extends ImplicitRigs {
  override implicit val Integers: Ring[Int] = Gadgets.Integers
  override implicit val Rationals: Ring[Fraction[Int]] = Gadgets.Rationals
  override implicit val BigInts: Ring[BigInt] = Gadgets.BigIntegers
  override implicit val BigRationals: Ring[Fraction[BigInt]] = Gadgets.BigRationals  
  override implicit val Doubles: Ring[Double] = Gadgets.Doubles
  override implicit val RationalPolynomials: Ring[Polynomial[Fraction[Int]]] = Gadgets.RationalPolynomials
}

object Ring extends ImplicitRings