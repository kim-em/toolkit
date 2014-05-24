package net.tqft.toolkit.algebra.mathematica

import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.mathematica._
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial

object Determinant {
  object ofMultivariableRationalFunctionMatrix {
    implicit class Determinant[I: IntegerModel](m: Seq[Seq[MultivariableRationalFunction[Fraction[I], String]]]) {
      def determinant: MultivariableRationalFunction[Fraction[I], String] = {
        implicit def vmf = MathematicaForm.BareStringMathematicaForm
        import Polynomials._
        import Mathematica._
        val input = FullFormExpression(SymbolExpression("Det"),
          Seq(m.map(r => r.map({ x: MultivariableRationalFunction[Fraction[I], String] => x: Expression_ }))))

        (input.evaluate): MultivariableRationalFunction[Fraction[I], String]
      }
    }
  }
  object ofMultivariablePolynomialMatrix {
    implicit class Determinant[I: IntegerModel](m: Seq[Seq[MultivariablePolynomial[Fraction[I], String]]]) {
      def determinant: MultivariablePolynomial[Fraction[I], String] = {
        implicit def vmf = MathematicaForm.BareStringMathematicaForm
        import Polynomials._
        import Mathematica._
        val input = FullFormExpression(SymbolExpression("Det"),
          Seq(m.map(r => r.map({ x: MultivariablePolynomial[Fraction[I], String] => x: Expression_ }))))

        (input.evaluate): MultivariablePolynomial[Fraction[I], String]
      }
    }
  }
}