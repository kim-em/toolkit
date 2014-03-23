package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.polynomials.Polynomial

object Gadgets {
  def ringOfIntegers = implicitly[Ring[Int]]
  def doublePolynomialAlgebra = implicitly[Algebra[Double, Polynomial[Double]]]
  def rationalFunctions = implicitly[Ring[RationalFunction[Int]]]

  def one: RationalFunction[Int] = 1

}