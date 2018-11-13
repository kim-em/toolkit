package net.tqft.toolkit.algebra.mathematica

import org.scalatest._
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial

class GroebnerBasisTest extends FlatSpec with Matchers {

  "GroebnerBasis" should "work via Mathematica" in {
    import mathematica.GroebnerBasis._

    // x^2 + x y, y^3 + x y^2

    val polynomials: Seq[MultivariablePolynomial[Fraction[Int], String]] = Seq(
      MultivariablePolynomial(Map(Map("x" -> 2) -> 1, Map("x" -> 1, "y" -> 1) -> 1)),
      MultivariablePolynomial(Map(Map("y" -> 3) -> 1, Map("x" -> 1, "y" -> 2) -> 1)))

    implicit def variableOrderings: Ordering[String] = Ordering.String.reverse
    val result = polynomials.computeGroebnerBasis

    import MathematicaForm._
    println(result.toMathematicaInputString)
  }
}
