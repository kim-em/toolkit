package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

object Gadgets {
  def algebraOfRationalPolynomials_0 = implicitly[Polynomials[Fraction[Int]]]
  def algebraOfRationalPolynomials_1 = implicitly[PolynomialsOverField[Fraction[Int]]]

  def orderedEuclideanRingOfRationalPolynomials = implicitly[OrderedEuclideanRing[Polynomial[Fraction[Int]]]]
  def euclideanRingOfRationalPolynomials = implicitly[EuclideanRing[Polynomial[Fraction[Int]]]]
  def ringOfRationalPolynomials = implicitly[Ring[Polynomial[Fraction[Int]]]]

  def ringOfPolynomials_0 = implicitly[Ring[Polynomial[Int]]]
  def ringOfPolynomials_1 = implicitly[GCDRing[Polynomial[Int]]]
  def ringOfDoublePolynomials = implicitly[Ring[Polynomial[Double]]]
  def ringOfPolynomials_2[A: Ring] = implicitly[Ring[Polynomial[A]]]
  
  def rationalFunctions = implicitly[Ring[RationalFunction[Fraction[Int]]]]
  def rationalFunctionsOver[B: Field] = implicitly[Ring[RationalFunction[B]]]

  def one_0 = 1
  def one_1: Fraction[Int] = 1
  def one_2: Polynomial[Fraction[Int]] = 1
  def one_3: Fraction[Polynomial[Int]] = 1

  def multivariablePolynomials = implicitly[MultivariablePolynomialAlgebra[Int, String]]
  def rigOfMultivariablePolynomials = implicitly[Rig[MultivariablePolynomial[Int, String]]]
  def ringOfMultivariablePolynomials = implicitly[Ring[MultivariablePolynomial[Int, String]]]

  def euclideanRingOfMultivariablePolynomials = implicitly[GCDRing[MultivariablePolynomial[Fraction[Int], String]]]
  def multivariableRationalFunctions = implicitly[Field[MultivariableRationalFunction[Fraction[Int], String]]]
}