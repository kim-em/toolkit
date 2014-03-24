package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

object Gadgets {
  def algebraOfRationalPolynomials_0 = implicitly[PolynomialAlgebra[Fraction[Int]]]
  def algebraOfRationalPolynomials_1 = implicitly[PolynomialAlgebraOverField[Fraction[Int]]]
  def algebraOfRationalPolynomials_2 = implicitly[PolynomialAlgebraOverOrderedField[Fraction[Int]]]
  def ordereedEuclideanRingOfRationalPolynomials = implicitly[OrderedEuclideanRing[Polynomial[Fraction[Int]]]]
  def euclideanRingOfRationalPolynomials = implicitly[EuclideanRing[Polynomial[Fraction[Int]]]]
  def ringOfRationalPolynomials = implicitly[Ring[Polynomial[Fraction[Int]]]]

  def ringOfPolynomials = implicitly[Ring[Polynomial[Int]]]
  def ringOfDoublePolynomials = implicitly[Ring[Polynomial[Double]]]
  //  def doublePolynomialAlgebra = implicitly[Algebra[Double, Polynomial[Double]]]
  
  def rationalFunctions = implicitly[Ring[RationalFunction[Int]]]
  
  def one_0 = 1
  def one_1: Fraction[Int] = 1
  def one_2: Polynomial[Fraction[Int]] = 1
  def one_3: Fraction[Polynomial[Fraction[Int]]] = 1
  def one_4: RationalFunction[Int] = 1

  def multivariablePolynomials = implicitly[MultivariablePolynomialAlgebra[Int, String]]
  def rigOfMultivariablePolynomials = implicitly[Rig[MultivariablePolynomial[Int, String]]]
  def ringOfMultivariablePolynomials = implicitly[Ring[MultivariablePolynomial[Int, String]]]
  
  def euclideanRingOfMultivariablePolynomials = implicitly[EuclideanRing[MultivariablePolynomial[Fraction[Int], String]]]
  
  def multivariableRationalFunctions = implicitly[Field[MultivariableRationalFunction[Int, String]]]

}