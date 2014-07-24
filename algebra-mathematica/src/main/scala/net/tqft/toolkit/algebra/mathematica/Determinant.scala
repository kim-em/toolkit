package net.tqft.toolkit.algebra.mathematica

import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.mathematica._
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.SHA1

object Determinant {

  private val cache = scala.collection.mutable.Map[String, String]()

  abstract class Determinant[I: IntegerModel, R](m: Seq[Seq[R]]) {
    def liftConstant(i: I): R
    def toExpression(r: R): Expression
    def fromExpression(e: Expression): R

    def determinant: R = {
      if (m.isEmpty) {
        liftConstant(implicitly[IntegerModel[I]].one)
      } else {
        import Polynomials._
        import Mathematica._

        implicit object WrappingMathematicaForm extends MathematicaForm[String] {
          override def toMathematicaInputString(s: String) = "a[\"" + s + "\"]"
          override def toString = "net.tqft.toolkit.algebra.mathematica.Determinant...WrappingMathematicaForm"
        }

        val input = FullFormExpression(SymbolExpression("Det"),
          Seq(m.map(r => r.map(toExpression))))
        val unwrappedInput = FullFormExpression(SymbolExpression("ReplaceAll"), Seq(input, Expression.expression.fromInputForm("a[x_] :> x")))
        fromExpression(unwrappedInput.evaluate)
      }
    }

    def cachedDeterminant: R = {
      import MathematicaForm._
      val hash = SHA1(m.map(_.map(toExpression)).toMathematicaInputString)
      val stringResult = cache.getOrElseUpdate(hash, toExpression(determinant).toInputForm)
      fromExpression(implicitly[MathematicaExpression[Expression]].fromInputForm(stringResult))
    }
  }

  object ofMultivariableRationalFunctionMatrix {
    implicit class implementation[I: IntegerModel](m: Seq[Seq[MultivariableRationalFunction[Fraction[I], String]]]) extends Determinant[I, MultivariableRationalFunction[Fraction[I], String]](m) {
      import Polynomials._

      def liftConstant(i: I) = i
      def toExpression(r: MultivariableRationalFunction[Fraction[I], String]) = r
      def fromExpression(e: Expression) = e
    }
  }
  object ofMultivariablePolynomialMatrix {
    implicit class implementation[I: IntegerModel](m: Seq[Seq[MultivariablePolynomial[Fraction[I], String]]]) extends Determinant[I, MultivariablePolynomial[Fraction[I], String]](m) {
      import Polynomials._

      def liftConstant(i: I) = i
      def toExpression(r: MultivariablePolynomial[Fraction[I], String]) = r
      def fromExpression(e: Expression) = e
    }
  }
}