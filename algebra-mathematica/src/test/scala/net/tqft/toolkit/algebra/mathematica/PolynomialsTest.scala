package net.tqft.toolkit.algebra.mathematica2

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.mathematica.Expression_

@RunWith(classOf[JUnitRunner])
class PolynomialsTest extends FlatSpec with Matchers {

  "multivariablePolynomialToExpression" should "convert a multivariable polynomial to something Mathematica understands" in {
    import Polynomials._
	  val p = MultivariablePolynomial(Map(Map("x" -> 2, "y" -> 1) -> 7, Map("x" ->1) -> 2))
	  println(p)
	  println(p: Expression_)
	  println((p: Expression_): MultivariablePolynomial[Int, String])
  }
}
