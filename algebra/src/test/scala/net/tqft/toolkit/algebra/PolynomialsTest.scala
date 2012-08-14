package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PolynomialsTest extends FlatSpec with ShouldMatchers {

  "Polynomial arithmetic" should "be correct" in {
    import Gadgets.bigIntegersAsBigRationals
    import Implicits.integersAsRationals
    //      import Implicits.asConstantPolynomial
    import AlgebraicNotation._

    val p: Polynomial[Fraction[Int]] = Polynomial(2 -> Fraction(1, 1), 1 -> Fraction(3, 1), 0 -> Fraction(1, 1))
    val q: Polynomial[Fraction[Int]] = Polynomial(1 -> Fraction(1, 1), 0 -> Fraction(-2, 1))

    (p % q) should equal(Polynomial(0 -> Fraction(11, 1)))

    val a: Polynomial[Int] = Polynomial(3 -> 1, 2 -> -8, 1 -> 21, 0 -> -18)
    Polynomials.evaluationAt(5).apply(a) should equal(12)

    val z: Polynomial[BigInt] = Polynomial(3 -> BigInt(1), 1 -> BigInt(16), 0 -> BigInt(-12), 2 -> BigInt(-7))
    val bigN = Fraction(BigInt(499), BigInt(100))
    (Polynomials.over(bigIntegersAsBigRationals) andThen Polynomials.evaluationAt(bigN))(z) should equal(Fraction(BigInt(17790799), BigInt(1000000)))
  }

  "cyclotomic" should "give the cyclotomic polynomials" in {
    Polynomial.cyclotomic[Fraction[Int]](1) should equal(Polynomial(0 -> Fraction(-1, 1), 1 -> Fraction(1, 1)))
    Polynomial.cyclotomic[Fraction[Int]](2) should equal(Polynomial(0 -> Fraction(1, 1), 1 -> Fraction(1, 1)))
    Polynomial.cyclotomic[Fraction[Int]](3) should equal(Polynomial(0 -> Fraction(1, 1), 1 -> Fraction(1, 1), 2 -> Fraction(1, 1)))
    Polynomial.cyclotomic[Fraction[Int]](4) should equal(Polynomial(0 -> Fraction(1, 1), 2 -> Fraction(1, 1)))
    Polynomial.cyclotomic[Fraction[Int]](5) should equal(Polynomial(0 -> Fraction(1, 1), 1 -> Fraction(1, 1), 2 -> Fraction(1, 1), 3 -> Fraction(1, 1), 4 -> Fraction(1, 1)))
    Polynomial.cyclotomic[Fraction[Int]](6) should equal(Polynomial(0 -> Fraction(1, 1), 1 -> Fraction(-1, 1), 2 -> Fraction(1, 1)))

  }
}



