package net.tqft.toolkit.algebra.polynomials

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.numberfields.NumberField

@RunWith(classOf[JUnitRunner])
class MinimalPolynomialTest extends FlatSpec with Matchers {

  "minimal polynomial" should "be correct" in {
    val sqrt13 = Polynomial(2 -> Fraction(1, 1), 0 -> Fraction(-13, 1))
    val L = Polynomial(1 -> Fraction(1, 2), 0 -> Fraction(5, 2))
    val numberField = NumberField(sqrt13)
    import MinimalPolynomial._
    numberField.minimalPolynomial(L) should equal(Polynomial(0 -> 3, 1-> -5, 2-> 1))
    numberField.integer_?(L) should equal(true)
    numberField.integer_?(Polynomial(1 -> Fraction(1, 2), 0 -> Fraction(5, 1))) should equal(false)
    numberField.dNumber_?(L) should equal(false)
    numberField.dNumber_?(Polynomial(1 -> Fraction(1, 2), 0 -> Fraction(3, 2))) should equal(true)
  }

}
