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
      e match {
        case FullFormExpression(SymbolExpression("Times"), factors) => polynomials.product(factors.map(expressionToMonomial))
        case e => expressionToMonomial(e)
      }
    }
    
    def expressionToMonomial(e: Expression_): MultivariablePolynomial[I, String] = {
      e match {
        case FullFormExpression(SymbolExpression("Power"), Seq(v, IntegerExpression(i))) => polynomials.monomial(Map(expressions.toInputForm(v) -> i.intValue()))
        case IntegerExpression(i) => polynomials.constant(integers.fromBigInt(i.toBigInteger()))
        case e => polynomials.monomial(expressions.toInputForm(e))
      }
    }

    e match {
      case FullFormExpression(SymbolExpression("Plus"), terms) => polynomials.sum(terms.map(expressionToMultivariableMonomial))
      case e => expressionToMultivariableMonomial(e)
    }
  }

}