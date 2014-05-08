package net.tqft.toolkit.algebra.mathematica

import scala.language.implicitConversions
import net.tqft.toolkit.mathematica.Expression_
import net.tqft.toolkit.mathematica.FullFormExpression
import net.tqft.toolkit.mathematica.SymbolExpression
import net.tqft.toolkit.mathematica.IntegerExpression
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebra
import net.tqft.toolkit.mathematica.MathematicaExpression
import net.tqft.toolkit.mathematica.Symbols

object Polynomials {
  implicit def multivariablePolynomialToExpression[A: MathematicaForm, V: MathematicaForm](p: MultivariablePolynomial[A, V]): Expression_ = {
    // TODO why isn't this available implicitly?
    def mf = MathematicaForm.multivariablePolynomialMathematicaForm[A, V]
    Expression_.expression.fromInputForm(mf.toMathematicaInputString(p))
  }

  implicit def expressionToMultivariablePolynomial[I: IntegerModel](e: Expression_): MultivariablePolynomial[I, String] = {
    val integers = implicitly[IntegerModel[I]]
    val polynomials = implicitly[MultivariablePolynomialAlgebra[I, String]]
    val expressions = implicitly[MathematicaExpression[Expression_]]

    def expressionToMultivariableMonomial(e: Expression_): MultivariablePolynomial[I, String] = {
//      println(s"expressionToMultivariableMonomial($e)")

      e match {
        case FullFormExpression(SymbolExpression("Times"), Seq(IntegerExpression(i), factors)) => polynomials.multiplyByInt(expressionToMultivariableMonomial(factors), i.intValue)
        case FullFormExpression(SymbolExpression("Times"), factors) => polynomials.product(factors.map(expressionToMonomial))
        case e => expressionToMonomial(e)
      }
    }

    def expressionToMonomial(e: Expression_): MultivariablePolynomial[I, String] = {
//      println(s"expressionToMonomial($e)")

      e match {
        case FullFormExpression(SymbolExpression("Power"), Seq(SymbolExpression(v), IntegerExpression(i))) => polynomials.monomial(Map(v -> i.intValue))
        case IntegerExpression(i) => polynomials.constant(integers.fromBigInt(i.toBigInteger()))
        case SymbolExpression(v) => polynomials.monomial(v)
      }
    }

//    println(s"expressionToMultivariablePolynomial($e)")
    e match {
      case FullFormExpression(SymbolExpression("Plus"), terms) => polynomials.sum(terms.map(expressionToMultivariableMonomial))
      case e => expressionToMultivariableMonomial(e)
    }
  }

}