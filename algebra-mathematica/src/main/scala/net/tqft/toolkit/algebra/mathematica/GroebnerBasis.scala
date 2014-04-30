package net.tqft.toolkit.algebra.mathematica2

import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.mathematica.Symbols
import net.tqft.toolkit.mathematica.SymbolExpression
import net.tqft.toolkit.mathematica.FullFormExpression
import net.tqft.toolkit.mathematica.Expression_
import net.tqft.toolkit.mathematica.Mathematica
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.polynomials.GroebnerBasisOperations

object GroebnerBasis extends GroebnerBasisOperations {
  implicit def algorithm[I: IntegerModel]: net.tqft.toolkit.algebra.polynomials.GroebnerBasis[I, String] = new net.tqft.toolkit.algebra.polynomials.GroebnerBasis[I, String] {
    implicit def vmf = MathematicaForm.BareStringMathematicaForm
    override def apply(polynomials: Seq[MultivariablePolynomial[I, String]])(implicit order: Ordering[String]) = {
      val variables = polynomials.flatMap(_.variables).distinct.sorted
      println(variables)
      import Polynomials._
      import Mathematica._
      val result = FullFormExpression(SymbolExpression("GroebnerBasis"),
        Seq(
          Symbols.List(polynomials.map(x => (x: Expression_)): _*),
          Symbols.List(variables.map(v => Expression_.expression.fromInputForm(vmf.toMathematicaInputString(v))): _*))).evaluate
      result match {
        case Symbols.List(arguments @ _*) => arguments.map(x => (x: MultivariablePolynomial[I, String]))
      }
    }
  }
}