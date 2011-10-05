package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PolynomialsTest extends FlatSpec with ShouldMatchers {
  
  "Polynomial arithmetic" should "be correct" in {	  
      import Implicits.Integers
      import Implicits.BigIntegers
      import Implicits.BigRationals
      import Gadgets.bigIntegersAsBigRationals
      import Implicits.Rationals
      import Implicits.IntegerPolynomials
      import Implicits.RationalPolynomials
      import Implicits.integersAsRationals
//      import Implicits.asConstantPolynomial
	  import AlgebraicNotation._
	  
	  val p: Polynomial[Fraction[Int]] = Polynomial(2 -> Fraction(1, 1), 1 -> Fraction(3, 1), 0 -> Fraction(1, 1))
	  val q: Polynomial[Fraction[Int]] = Polynomial(1 -> Fraction(1, 1), 0 -> Fraction(-2, 1))
	  
	  (p % q) should equal(Polynomial(0 -> Fraction(11, 1)))
	  
      val a: Polynomial[Int] = Polynomial(3 -> 1, 2 -> -8, 1 -> 21, 0 -> -18)
      Polynomials.evaluateAt(5)(Integers)(a) should equal(12)
      
      val z: Polynomial[BigInt] = Polynomial(3->BigInt(1), 1->BigInt(16),0 -> BigInt(-12), 2 -> BigInt(-7))
      val bigN = Fraction(BigInt(499), BigInt(100))
       (Polynomials.over(bigIntegersAsBigRationals) andThen Polynomials.evaluateAt(bigN))(z) should equal(Fraction(BigInt(17790799),BigInt(1000000)))
  }
}
