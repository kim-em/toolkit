package net.tqft.toolkit.algebra.mathematica

import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.mathematica.Symbols
import net.tqft.toolkit.mathematica.SymbolExpression
import net.tqft.toolkit.mathematica.FullFormExpression
import net.tqft.toolkit.mathematica.Expression_
import net.tqft.toolkit.mathematica.Mathematica
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.polynomials.GroebnerBasisOperations
import net.tqft.toolkit.algebra.Fraction

object GroebnerBasis extends GroebnerBasisOperations {
  implicit def algorithm[I: IntegerModel]: net.tqft.toolkit.algebra.polynomials.GroebnerBasis[Fraction[I], String] = new net.tqft.toolkit.algebra.polynomials.GroebnerBasis[Fraction[I], String] {
    implicit object WrappingMathematicaForm extends MathematicaForm[String] {
      override def toMathematicaInputString(s: String) = "a[\"" + s + "\"]"
      override def toString = "GroebnerBasis.algorithm.WrappingMathematicaForm"
    }
    override def apply(polynomials: Seq[MultivariablePolynomial[Fraction[I], String]])(implicit order: Ordering[String]) = {
      val variables = polynomials.flatMap(_.variables).distinct.sorted.reverse
      import Polynomials._
      import Mathematica._
      val input = FullFormExpression(SymbolExpression("GroebnerBasis"),
        Seq(
          Symbols.List(polynomials.map(x => (x: Expression_)): _*),
          Symbols.List(variables.map(v => Expression_.expression.fromInputForm(WrappingMathematicaForm.toMathematicaInputString(v))): _*)))
      val unwrappedInput = FullFormExpression(SymbolExpression("ReplaceAll"), Seq(input, Expression_.expression.fromInputForm("a[x_] :> x")))
//      println(unwrappedInput.toInputForm)
      val result = unwrappedInput.evaluate
      result match {
        case Symbols.List(arguments @ _*) => arguments.map(x => (x: MultivariablePolynomial[Fraction[I], String]))
      }
    }
  }
}