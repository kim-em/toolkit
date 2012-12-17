package net.tqft.toolkit.algebra.numberfields

import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.Fields
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.OrderedField
import net.tqft.toolkit.algebra.ApproximateReals

// needs lots of work!
object RealNumberField {
  def apply[I: IntegerModel, D: ApproximateReals](minimalPolynomial: Polynomial[I], approximation: D, epsilon: D): RealNumberField[I, D] = {
    new RealNumberField[I, D] {
      override val generator = minimalPolynomial.coefficientsAsFractions
      override val goodEnoughApproximation = approximation

      override var bestApproximation = approximation
      override var errorBound = epsilon

      override val integers = implicitly[IntegerModel[I]]
      override val approximateReals = implicitly[ApproximateReals[D]]
      override val coefficientField = Fields.fieldOfFractions(integers)
    }
  }
}

trait RealNumberField[I, D] extends NumberField[Fraction[I]] with OrderedField[Polynomial[Fraction[I]]] {
  val goodEnoughApproximation: D
  implicit val integers: IntegerModel[I]
  lazy val rationals = Fields.fieldOfFractions(integers)
  val approximateReals: ApproximateReals[D]

  protected var bestApproximation: D
  protected var errorBound: D
  private def errorBoundForPower(k: Int): D = k match {
    case 0 => approximateReals.zero
    case k => approximateReals.multiplyByInt(approximateReals.multiply(errorBound, approximateReals.power(bestApproximation, k - 1)), k)
  }

  def improveErrorBound = ???

  def evaluateAt(d: D)(p: Polynomial[Fraction[I]]): D = approximateReals.sum(for ((i, c) <- p.terms) yield approximateReals.multiply(evaluateFraction(c), approximateReals.power(d, i)))
  def evaluateFraction(x: Fraction[I]) = approximateReals.quotient(approximateReals.fromInteger(x.numerator), approximateReals.fromInteger(x.denominator))

  def approximateWithin(epsilon: D)(p: Polynomial[Fraction[I]]): D = {
    for (i <- 0 until degree) {
      while (approximateReals.compare(approximateReals.multiply(errorBoundForPower(i), evaluateFraction(generator.coefficientOf(i).getOrElse(rationals.zero))), approximateReals.quotientByInt(epsilon, degree)) > 0) {
        improveErrorBound
      }
    }
    evaluateAt(bestApproximation)(p)
  }

  override def compare(x: Polynomial[Fraction[I]], y: Polynomial[Fraction[I]]) = {
    if (x == y) {
      0
    } else {
      var epsilon = approximateReals.fromDouble(0.0001)
      def gap = approximateReals.subtract(approximateWithin(epsilon)(x), approximateWithin(epsilon)(y))
      while (approximateReals.compare(approximateReals.abs(gap), approximateReals.multiplyByInt(epsilon, 4)) < 0) {
        epsilon = approximateReals.quotientByInt(epsilon, 10)
      }
      approximateReals.compare(gap, approximateReals.zero).ensuring(_ != 0)
    }
  }
}