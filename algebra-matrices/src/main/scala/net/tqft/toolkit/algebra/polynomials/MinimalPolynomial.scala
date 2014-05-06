package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.numberfields.NumberField
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.AlgebraicNotation
import net.tqft.toolkit.algebra.GCDRing

// usage... if x: Polynomial[A], and we have ring: NumberField[A], then ring.minimalPolynomial(x): Polynomial[A]
//      ... if ring is implicit, x.minimalPolynomial: Polynomial[A]

object MinimalPolynomial {

  implicit class PolynomialOperations[A](p: Polynomial[Fraction[A]])(implicit ring: GCDRing[A], numberField: NumberField[Fraction[A]]) {
    def minimalPolynomial = implicitly[NumberField[Fraction[A]]].minimalPolynomial(p)
  }
  implicit class IntegerPolynomialOperations[I](p: Polynomial[Fraction[I]])(implicit integers: IntegerModel[I], numberField: NumberField[Fraction[I]]) {
    def integer_? = numberField.integer_?(p)
    def dNumber_? = numberField.dNumber_?(p)
  }

  implicit class NumberFieldWithMinimalPolynomial[A:GCDRing](numberField: NumberField[Fraction[A]]) {
    def minimalPolynomial(p: Polynomial[Fraction[A]]): Polynomial[A] = {
      val powers = Seq.tabulate(numberField.degree + 1)(k => numberField.power(p, k))
      val coefficients = powers.map(q => numberField.toSeq(q).padTo(numberField.degree + 1, numberField.ring.zero))
      implicit def field = numberField.ring
      val nullSpace = Matrix(numberField.degree + 1, coefficients.transpose).nullSpace
      val rationalMinimalPolynomial = nullSpace.map(s => SeqPolynomial(s: _*)).sortBy(_.maximumDegree).head
      val c = implicitly[GCDRing[A]].lcm(numberField.toSeq(rationalMinimalPolynomial).map(_.denominator):_*)
      rationalMinimalPolynomial.mapValues(x => numberField.ring.multiply(x, c).numerator)
    }
  }

  implicit class IntegerNumberFieldOperations[I: IntegerModel](numberField: NumberField[Fraction[I]]) {
    private def integers = implicitly[IntegerModel[I]]
    private implicit def field = numberField.ring
    import AlgebraicNotation._

    def integer_?(p: Polynomial[Fraction[I]]): Boolean = {
      numberField.minimalPolynomial(p).leadingCoefficient == integers.one
    }

    def dNumber_?(p: Polynomial[Fraction[I]]): Boolean = {
      val mpc = numberField.minimalPolynomial(p).toSeq
      val n = mpc.size - 1
      val constant = mpc.head
      mpc.zipWithIndex.forall({
        case (a, i) => {
          integers.exactQuotientOption(integers.power(a, n), integers.power(constant, n - i)).nonEmpty
        }
      })
    }
  }

}