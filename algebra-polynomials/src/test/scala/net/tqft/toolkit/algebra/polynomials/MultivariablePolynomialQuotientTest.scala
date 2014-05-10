package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class MultivariablePolynomialQuotientTest extends FlatSpec with Matchers {
  "Q[x,y]/{x^2=y}" should "behave sensibly" in {

    import AlgebraicNotation._

    val x = MultivariablePolynomial(Map(Map("x" -> 1) -> Fraction(1, 1)))
    val y = MultivariablePolynomial(Map(Map("y" -> 1) -> Fraction(1, 1)))

    {
      val quotient = MultivariablePolynomialAlgebras.quotient(
        Seq((x ^ 2) - y))

      quotient.monomialOrdering.compare(Map("x" -> 1), Map("y" -> 1)) < 0 should be(true)
      quotient.monomialOrdering.compare(Map("x" -> 1), Map("y" -> 2)) < 0 should be(true)
      quotient.monomialOrdering.compare(Map("x" -> 2), Map("y" -> 1)) < 0 should be(true)
      quotient.monomialOrdering.compare(Map("x" -> 2, "y" -> 1), Map("y" -> 2)) < 0 should be(true)

      quotient.normalForm((x ^ 5) * y + (x ^ 2) * (y ^ 2)) should equal((x ^ 7) + (x ^ 6))
    }
    {
      val quotient = MultivariablePolynomialAlgebras.quotient(
        Seq(x - (y ^ 2)))

      quotient.normalForm((y ^ 5) * x + (x ^ 2) * (y ^ 2)) should equal((x ^ 3) * y + (x ^ 3))
    }
  }
}