package net.tqft.toolkit.algebra.mathematica

import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.mathematica.Symbols
import net.tqft.toolkit.mathematica.SymbolExpression
import net.tqft.toolkit.mathematica.FullFormExpression
import net.tqft.toolkit.mathematica.Expression
import net.tqft.toolkit.mathematica.IntegerExpression
import net.tqft.toolkit.mathematica.Mathematica
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.polynomials.GroebnerBasisOperations
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.Factorization
import net.tqft.toolkit.algebra.FactorizationAlgorithm
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebraOverField

object FactorWrapped extends FactorizationAlgorithm {
  implicit def algorithm[I: IntegerModel]: Factorization[MultivariablePolynomial[Fraction[I], String]] = new Factorization[MultivariablePolynomial[Fraction[I], String]] {
    implicit object WrappingMathematicaForm extends MathematicaForm[String] {
      override def toMathematicaInputString(s: String) = "a[\"" + s + "\"]"
      override def toString = "FactorWrapped.algorithm.WrappingMathematicaForm"
    }
    override def factor(polynomial: MultivariablePolynomial[Fraction[I], String]) = {
      import Polynomials._
      import Mathematica._

      val input = FullFormExpression(SymbolExpression("Factor"),
        Seq(polynomial))
      val unwrappedInput = FullFormExpression(SymbolExpression("ReplaceAll"), Seq(input, Expression.expression.fromInputForm("a[x_] :> x")))
      //      println(unwrappedInput.toInputForm)
      val result = unwrappedInput.evaluate

      def unpack(e: Expression): Map[MultivariablePolynomial[Fraction[I], String], Int] = {
        e match {
          case Symbols.Power(p, k: IntegerExpression) => Map((p: MultivariablePolynomial[Fraction[I], String]) -> k.value.intValue)
          case Symbols.Times(arguments @ _*) => arguments.flatMap(unpack).toMap
          case e => Map((e: MultivariablePolynomial[Fraction[I], String]) -> 1)
        }
      }

      unpack(result)
    }
  }

  implicit class polynomialAlgebraWithFactorization[I: IntegerModel](polynomials: MultivariablePolynomialAlgebraOverField[Fraction[I], String]) {
    def factor(p: MultivariablePolynomial[Fraction[I], String]) = p.factor
  }
}
object Factor extends FactorizationAlgorithm {
  implicit def algorithm[I: IntegerModel]: Factorization[MultivariablePolynomial[Fraction[I], String]] = new Factorization[MultivariablePolynomial[Fraction[I], String]] {
    override def factor(polynomial: MultivariablePolynomial[Fraction[I], String]) = {
      import Polynomials._
      import Mathematica._
      implicit def vmf = MathematicaForm.BareStringMathematicaForm

      val input = FullFormExpression(SymbolExpression("Factor"),
        Seq(polynomial))
        
        println(input.toInputForm)
        
      val result = input.evaluate

      def unpack(e: Expression): Map[MultivariablePolynomial[Fraction[I], String], Int] = {
        e match {
          case Symbols.Power(p, k: IntegerExpression) => Map((p: MultivariablePolynomial[Fraction[I], String]) -> k.value.intValue)
          case Symbols.Times(arguments @ _*) => arguments.flatMap(unpack).toMap
          case e => Map((e: MultivariablePolynomial[Fraction[I], String]) -> 1)
        }
      }

      unpack(result)
    }
  }

  implicit class polynomialAlgebraWithFactorization[I: IntegerModel](polynomials: MultivariablePolynomialAlgebraOverField[Fraction[I], String]) {
    def factor(p: MultivariablePolynomial[Fraction[I], String]) = p.factor
  }
}