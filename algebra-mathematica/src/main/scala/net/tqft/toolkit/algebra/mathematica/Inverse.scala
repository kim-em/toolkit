package net.tqft.toolkit.algebra.mathematica

import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.mathematica._

object Inverse {
  object ofMultivariableRationalFunctionMatrix {
    implicit class Inverse[I: IntegerModel](m: Seq[Seq[MultivariableRationalFunction[Fraction[I], String]]]) {
      def inverse: Seq[Seq[MultivariableRationalFunction[Fraction[I], String]]] = {
        implicit def vmf = MathematicaForm.BareStringMathematicaForm
        import Polynomials._
        import Mathematica._
        val input = FullFormExpression(SymbolExpression("Together"), Seq(FullFormExpression(SymbolExpression("Inverse"),
          Seq(m.map(r => r.map({ x: MultivariableRationalFunction[Fraction[I], String] => x: Expression }))))))

       input.evaluate match {
          case Symbols.List(rows @ _*) => rows.map(r => r match {
            case Symbols.List(entries @ _*) => entries.map(x => x: MultivariableRationalFunction[Fraction[I], String])
          })
        }
      }
    }
  }
}