package net.tqft.toolkit.algebra.mathematica

import scala.language.implicitConversions
import net.tqft.toolkit.mathematica.Expression_
import net.tqft.toolkit.mathematica.FullFormExpression
import net.tqft.toolkit.mathematica.SymbolExpression
import net.tqft.toolkit.mathematica.StringExpression
import net.tqft.toolkit.mathematica.IntegerExpression
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebra
import net.tqft.toolkit.mathematica.MathematicaExpression
import net.tqft.toolkit.mathematica.Symbols
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction
import org.apfloat.Apint
import net.tqft.toolkit.algebra.Field

object Polynomials {
  implicit def multivariablePolynomialToExpression[A: MathematicaForm, V: MathematicaForm](p: MultivariablePolynomial[A, V]): Expression_ = {
    // TODO why isn't this available implicitly?
    def mf = MathematicaForm.multivariablePolynomialMathematicaForm[A, V]
    Expression_.expression.fromInputForm(mf.toMathematicaInputString(p))
  }
  implicit def multivariableRationalFunctionToExpression[A: MathematicaForm, V: MathematicaForm](p: MultivariableRationalFunction[A, V]): Expression_ = {
    val en = multivariablePolynomialToExpression(p.numerator)
    val ed = multivariablePolynomialToExpression(p.denominator)

    if (ed == IntegerExpression(new Apint(1))) {
      en
    } else {
      Symbols.Times(
        en,
        Symbols.Power(ed, IntegerExpression(new Apint(-1))))
    }
  }

  implicit def expressionToMultivariablePolynomial[I: IntegerModel](e: Expression_): MultivariablePolynomial[Fraction[I], String] = {
    val integers = implicitly[IntegerModel[I]]
    val rationals = implicitly[Field[Fraction[I]]]
    val polynomials = implicitly[MultivariablePolynomialAlgebra[Fraction[I], String]]

    //    println(s"expressionToMultivariablePolynomial($e)")
    e match {
      case Symbols.Plus(terms @ _*) => polynomials.sum(terms.map(expressionToMultivariablePolynomial[I]))
      case Symbols.Times(factors @ _*) => polynomials.product(factors.map(expressionToMultivariablePolynomial[I]))
      case Symbols.Power(IntegerExpression(x), IntegerExpression(i)) => polynomials.constant(rationals.power(integers.fromBigInt(x.toBigInteger()), i.intValue))
      case Symbols.Power(x, IntegerExpression(i)) => polynomials.power(expressionToMultivariablePolynomial[I](x), i.intValue)
      case IntegerExpression(i) => polynomials.constant(integers.fromBigInt(i.toBigInteger()))
      case SymbolExpression(v) => polynomials.monomial(v)
      case StringExpression(e) => polynomials.monomial(e)
      case _ => require(false); ???
    }
  }

  implicit def expressionToMultivariableRationalFunction[I: IntegerModel](e: Expression_): MultivariableRationalFunction[Fraction[I], String] = {
    //    println(s"expressionToMultivariableRationalFunction($e)")

    val rationalFunctions = implicitly[Field[MultivariableRationalFunction[Fraction[I], String]]]

    e match {
      case Symbols.Times(factors @ _*) => rationalFunctions.product(factors.map(f => f: MultivariableRationalFunction[Fraction[I], String]))
      case Symbols.Plus(terms @ _*) => rationalFunctions.sum(terms.map(f => f: MultivariableRationalFunction[Fraction[I], String]))
      case Symbols.Power(x, IntegerExpression(i)) => rationalFunctions.power(x: MultivariableRationalFunction[Fraction[I], String], i.intValue)
      case e => Fraction.whole(e: MultivariablePolynomial[Fraction[I], String])
    }
  }

}