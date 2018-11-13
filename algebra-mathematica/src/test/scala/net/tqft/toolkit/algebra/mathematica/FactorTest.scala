package net.tqft.toolkit.algebra.mathematica

import org.scalatest._
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial

class FactorTest extends FlatSpec with Matchers {

  "Factor" should "work via Mathematica" in {
    import mathematica.Factor._

    import AlgebraicNotation._

    val x = MultivariablePolynomial(Map(Map("x" -> 1) -> Fraction(1, 1)))
    val y = MultivariablePolynomial(Map(Map("y" -> 1) -> Fraction(1, 1)))

    
    val p  = (x^2)  + ((2 *: x) * y) + (y^2)
    
    p.factor should equal(Map(x + y -> 2))    
  }
}
