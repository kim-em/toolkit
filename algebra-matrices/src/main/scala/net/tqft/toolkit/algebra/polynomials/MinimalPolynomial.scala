package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.numberfields.NumberField
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.AlgebraicNotation

// usage... if x: Polynomial[A], and we have ring: NumberField[A], then ring.minimalPolynomial(x): Polynomial[A]
//      ... if ring is implicit, x.minimalPolynomial: Polynomial[A]

object MinimalPolynomial {

  implicit class PolynomialOperations[A:NumberField](p: Polynomial[A]) {
    def minimalPolynomial = implicitly[NumberField[A]].minimalPolynomial(p)
  }
  implicit class IntegerPolynomialOperations[I](p: Polynomial[Fraction[I]])(implicit integers: IntegerModel[I], numberField: NumberField[Fraction[I]]) {
    def integer_? = numberField.integer_?(p)
    def dNumber_? = numberField.dNumber_?(p)
  }

  implicit class NumberFieldWithMinimalPolynomial[A](numberField: NumberField[A]) {
    def minimalPolynomial(p: Polynomial[A]): Polynomial[A] = {
      val powers = Seq.tabulate(numberField.degree + 1)(k => numberField.power(p, k))
      val coefficients = powers.map(q => numberField.toSeq(q))
      implicit def field = numberField.ring
      val nullSpace = Matrix(numberField.degree + 1, coefficients).nullSpace
      nullSpace.map(s => SeqPolynomial(s:_*)).sortBy(_.maximumDegree).head
    }
  }

  implicit class IntegerNumberFieldOperations[I:IntegerModel](numberField: NumberField[Fraction[I]]) {
    def integer_?(p: Polynomial[Fraction[I]]): Boolean = {
      def integers = implicitly[IntegerModel[I]]
      implicit def field = numberField.ring
      import AlgebraicNotation._
      val mpc = numberField.minimalPolynomial(p).toSeq
      val lc = mpc.last
      mpc.dropRight(1).map(c => c / lc).forall(_.denominator == integers.one)
    }

    def dNumber_?(p: Polynomial[Fraction[I]]): Boolean = {
      ???
    }
  }

}