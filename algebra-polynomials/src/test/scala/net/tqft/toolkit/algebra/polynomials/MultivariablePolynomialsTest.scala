package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class MultivariablePolynomialsTest extends FlatSpec with Matchers {

  "gcd of zero and a monomial " should "be that monomial" in {
    val polynomials = implicitly[MultivariablePolynomialAlgebraOverGCDRing[Fraction[Int], String]]
    val m = polynomials.monomial(Map("d" -> 1), 1)
    val z = polynomials.zero
    polynomials.gcd(m, z) should equal(m)
    polynomials.gcd(z, m) should equal(m)
  }
  "gcd of 0 and x" should "be x" in {
    val polynomials = implicitly[MultivariablePolynomialAlgebraOverGCDRing[Fraction[Int], String]]
    val t = "t"
    val b = "b"
    val d = "d"
    val p1 = MultivariablePolynomial(Map(Map(t -> 1, d -> 1) -> Fraction(1, 1)))
    val p2 = MultivariablePolynomial(Map(Map(t -> 2, d -> 3) -> Fraction(1, 1), Map(b -> 2, d -> 2) -> Fraction(2, 1), Map(t -> 2, d -> 1) -> Fraction(-1, 1), Map(b -> 2, d -> 3) -> Fraction(-1, 1), Map(t -> 1, b -> 1, d -> 1) -> Fraction(2, 1)))
    polynomials.gcd(polynomials.zero, p1) should equal(p1)
    polynomials.gcd(p1, polynomials.zero) should equal(p1)
    polynomials.gcd(polynomials.zero, p2) should equal(p2)
    polynomials.gcd(p2, polynomials.zero) should equal(p2)
  }
  "gcd" should "not overflow badly" in {
    type C = Int

    val polynomials = implicitly[MultivariablePolynomialAlgebraOverGCDRing[C, String]]
    val b = "b"
    val d = "d"
    val p = MultivariablePolynomial[C, String](Map(Map(d -> 5) -> -4, Map(d -> 1) -> -2, Map() -> -1, Map(d -> 7) -> 1, Map(d -> 3) -> 5, Map(d -> 4) -> -1, Map(d -> 2) -> 2))
    val q = MultivariablePolynomial[C, String](Map(Map(b -> 4, d -> 2) -> 2, Map(b -> 4, d -> 3) -> -3, Map(b -> 4, d -> 6) -> 3, Map(b -> 4, d -> 5) -> 5, Map(b -> 4, d -> 7) -> -4, Map(b -> 4, d -> 1) -> 1, Map(b -> 4, d -> 4) -> -5, Map(b -> 4, d -> 8) -> 1))
    val g = polynomials.gcd(p, q)
    g should equal(MultivariablePolynomial[C, String](Map(Map(d -> 3) -> 1, Map(d -> 2) -> -2, Map() -> 1)))
  }
  "gcd" should "terminate" in {
    val polynomials = implicitly[MultivariablePolynomialAlgebraOverField[Fraction[BigInt], String]]
    val (p1, p2, p3, p4, p5, p6) = ("p1", "p2", "p3", "p4", "p5", "p6")
    val p = MultivariablePolynomial[Fraction[BigInt], String](Map(Map(p4 -> 4, p5 -> 2, p3 -> 17, p2 -> 4, p1 -> 1) -> Fraction(-21, 1), Map(p4 -> 8, p5 -> 3, p3 -> 7, p2 -> 9, p1 -> 1) -> Fraction(1, 2), Map(p4 -> 2, p5 -> 2, p3 -> 21, p2 -> 2, p1 -> 1) -> Fraction(-38, 1), Map(p4 -> 1, p5 -> 2, p3 -> 23, p2 -> 1, p1 -> 1) -> Fraction(-1, 2), Map(p4 -> 8, p5 -> 1, p3 -> 9, p2 -> 10, p1 -> 1) -> Fraction(-9, 2), Map(p4 -> 6, p5 -> 1, p3 -> 15, p2 -> 5, p1 -> 1) -> Fraction(595, 1), Map(p2 -> 13, p4 -> 12, p3 -> 3) -> Fraction(13, 1), Map(p4 -> 7, p5 -> 1, p3 -> 11, p2 -> 9, p1 -> 1) -> Fraction(18, 1), Map(p4 -> 5, p5 -> 1, p3 -> 14, p2 -> 6, p1 -> 1, p6 -> 1) -> Fraction(-266, 1), Map(p2 -> 9, p4 -> 8, p3 -> 11) -> Fraction(1287, 1), Map(p4 -> 2, p3 -> 19, p2 -> 4, p1 -> 1, p6 -> 2) -> Fraction(24, 1), Map(p4 -> 6, p5 -> 1, p3 -> 12, p2 -> 7, p1 -> 1, p6 -> 1) -> Fraction(196, 1), Map(p4 -> 7, p5 -> 2, p3 -> 11, p2 -> 7, p1 -> 1) -> Fraction(-226, 1), Map(p4 -> 7, p5 -> 3, p3 -> 9, p2 -> 8, p1 -> 1) -> Fraction(-4, 1), Map(p4 -> 6, p5 -> 2, p3 -> 13, p2 -> 6, p1 -> 1) -> Fraction(224, 1), Map(p4 -> 9, p5 -> 1, p3 -> 9, p2 -> 8, p1 -> 1) -> Fraction(-136, 1), Map(p2 -> 10, p1 -> 1, p4 -> 9, p3 -> 9) -> Fraction(-1, 2), Map(p2 -> 7, p4 -> 6, p3 -> 15) -> Fraction(1716, 1), Map(p4 -> 8, p5 -> 2, p3 -> 9, p2 -> 8, p1 -> 1) -> Fraction(277, 2), Map(p4 -> 2, p5 -> 1, p3 -> 21, p2 -> 4, p1 -> 1) -> Fraction(-14, 1), Map(p2 -> 9, p1 -> 1, p4 -> 8, p3 -> 11) -> Fraction(9, 2), Map(p4 -> 2, p3 -> 20, p2 -> 5, p1 -> 1, p6 -> 1) -> Fraction(-4, 1), Map(p4 -> 11, p5 -> 2, p3 -> 3, p2 -> 11, p1 -> 1) -> Fraction(-1, 1), Map(p4 -> 9, p3 -> 8, p2 -> 9, p1 -> 1, p6 -> 1) -> Fraction(3, 1), Map(p4 -> 5, p5 -> 2, p3 -> 15, p2 -> 5, p1 -> 1) -> Fraction(-112, 1), Map(p2 -> 3, p1 -> 1, p6 -> 1, p3 -> 24) -> Fraction(-1, 2), Map(p4 -> 10, p5 -> 2, p3 -> 5, p2 -> 10, p1 -> 1) -> Fraction(11, 1), Map(p2 -> 1, p1 -> 1, p4 -> 3, p3 -> 23) -> Fraction(417, 2), Map(p4 -> 9, p5 -> 1, p3 -> 6, p2 -> 10, p1 -> 1, p6 -> 1) -> Fraction(-3, 1), Map(p2 -> 9, p1 -> 1, p4 -> 11, p3 -> 7) -> Fraction(297, 1), Map(p4 -> 1, p5 -> 2, p3 -> 20, p2 -> 3, p1 -> 1, p6 -> 1) -> Fraction(-4, 1), Map(p2 -> 10, p4 -> 9, p3 -> 9) -> Fraction(-715, 1), Map(p2 -> 7, p1 -> 1, p4 -> 9, p3 -> 11) -> Fraction(1475, 1), Map(p1 -> 1, p6 -> 1, p3 -> 26) -> Fraction(1, 1), Map(p4 -> 1, p3 -> 22, p2 -> 4, p1 -> 1, p6 -> 1) -> Fraction(9, 2), Map(p4 -> 3, p5 -> 1, p3 -> 18, p2 -> 4, p1 -> 1, p6 -> 1) -> Fraction(-165, 1), Map(p5 -> 1, p3 -> 24, p2 -> 1, p1 -> 1, p6 -> 1) -> Fraction(-1, 1), Map(p4 -> 2, p5 -> 1, p3 -> 20, p2 -> 3, p1 -> 1, p6 -> 1) -> Fraction(31, 1), Map(p4 -> 3, p5 -> 3, p3 -> 17, p2 -> 4, p1 -> 1) -> Fraction(-28, 1), Map(p2 -> 5, p1 -> 1, p4 -> 7, p3 -> 15) -> Fraction(2410, 1), Map(p2 -> 3, p1 -> 1, p4 -> 2, p3 -> 23) -> Fraction(19, 2), Map(p4 -> 4, p5 -> 1, p3 -> 16, p2 -> 5, p1 -> 1, p6 -> 1) -> Fraction(246, 1), Map(p1 -> 1, p4 -> 2, p3 -> 25) -> Fraction(-29, 1), Map(p2 -> 7, p1 -> 1, p4 -> 6, p3 -> 15) -> Fraction(42, 1), Map(p2 -> 11, p4 -> 10, p3 -> 7) -> Fraction(286, 1), Map(p4 -> 6, p5 -> 3, p3 -> 11, p2 -> 7, p1 -> 1) -> Fraction(14, 1), Map(p2 -> 8, p1 -> 1, p4 -> 10, p3 -> 9) -> Fraction(-1545, 2), Map(p4 -> 6, p3 -> 14, p2 -> 6, p1 -> 1, p6 -> 1) -> Fraction(-196, 1), Map(p5 -> 2, p3 -> 22, p2 -> 2, p1 -> 1, p6 -> 1) -> Fraction(1, 2), Map(p4 -> 9, p5 -> 2, p3 -> 7, p2 -> 9, p1 -> 1) -> Fraction(-52, 1), Map(p4 -> 1, p3 -> 24, p2 -> 1, p1 -> 1, p6 -> 1) -> Fraction(-13, 1), Map(p2 -> 12, p4 -> 11, p3 -> 5) -> Fraction(-78, 1), Map(p4 -> 11, p5 -> 1, p3 -> 5, p2 -> 10, p1 -> 1) -> Fraction(-10, 1), Map(p2 -> 14, p4 -> 13, p3 -> 1) -> Fraction(-1, 1), Map(p4 -> 4, p5 -> 1, p3 -> 19, p2 -> 3, p1 -> 1) -> Fraction(459, 1), Map(p2 -> 2, p1 -> 1, p6 -> 2, p3 -> 23) -> Fraction(1, 1), Map(p4 -> 3, p5 -> 2, p3 -> 19, p2 -> 3, p1 -> 1) -> Fraction(84, 1), Map(p2 -> 1, p3 -> 27) -> Fraction(1, 1), Map(p2 -> 5, p1 -> 1, p4 -> 4, p3 -> 19) -> Fraction(63, 1), Map(p4 -> 6, p5 -> 1, p3 -> 13, p2 -> 8, p1 -> 1) -> Fraction(-42, 1), Map(p4 -> 8, p5 -> 1, p3 -> 11, p2 -> 7, p1 -> 1) -> Fraction(286, 1), Map(p5 -> 1, p1 -> 1, p4 -> 1, p3 -> 25) -> Fraction(1, 1), Map(p4 -> 10, p5 -> 1, p3 -> 7, p2 -> 9, p1 -> 1) -> Fraction(93, 2), Map(p2 -> 6, p4 -> 5, p3 -> 17) -> Fraction(-1287, 1), Map(p4 -> 1, p3 -> 21, p2 -> 3, p1 -> 1, p6 -> 2) -> Fraction(-11, 1), Map(p2 -> 4, p1 -> 1, p4 -> 3, p3 -> 21) -> Fraction(-38, 1), Map(p2 -> 8, p4 -> 7, p3 -> 13) -> Fraction(-1716, 1), Map(p2 -> 11, p1 -> 1, p4 -> 13, p3 -> 3) -> Fraction(13, 1), Map(p4 -> 2, p3 -> 22, p2 -> 2, p1 -> 1, p6 -> 1) -> Fraction(41, 2), Map(p2 -> 6, p1 -> 1, p4 -> 5, p3 -> 17) -> Fraction(-63, 1), Map(p2 -> 2, p1 -> 1, p4 -> 4, p3 -> 21) -> Fraction(-687, 1), Map(p4 -> 1, p5 -> 1, p3 -> 22, p2 -> 2, p1 -> 1, p6 -> 1) -> Fraction(8, 1), Map(p4 -> 7, p5 -> 1, p3 -> 13, p2 -> 6, p1 -> 1) -> Fraction(-464, 1), Map(p4 -> 7, p3 -> 12, p2 -> 7, p1 -> 1, p6 -> 1) -> Fraction(92, 1), Map(p4 -> 4, p5 -> 3, p3 -> 15, p2 -> 5, p1 -> 1) -> Fraction(35, 1), Map(p4 -> 12, p5 -> 1, p3 -> 3, p2 -> 11, p1 -> 1) -> Fraction(1, 1), Map(p4 -> 9, p5 -> 1, p3 -> 7, p2 -> 11, p1 -> 1) -> Fraction(1, 2), Map(p2 -> 2, p1 -> 1, p4 -> 1, p3 -> 25) -> Fraction(1, 2), Map(p4 -> 5, p5 -> 1, p3 -> 17, p2 -> 4, p1 -> 1) -> Fraction(-596, 1), Map(p2 -> 5, p4 -> 4, p3 -> 19) -> Fraction(715, 1), Map(p4 -> 5, p3 -> 16, p2 -> 5, p1 -> 1, p6 -> 1) -> Fraction(258, 1), Map(p2 -> 3, p4 -> 2, p3 -> 23) -> Fraction(78, 1), Map(p2 -> 2, p4 -> 1, p3 -> 25) -> Fraction(-13, 1), Map(p4 -> 4, p3 -> 18, p2 -> 4, p1 -> 1, p6 -> 1) -> Fraction(-213, 1), Map(p4 -> 2, p5 -> 1, p3 -> 23, p2 -> 1, p1 -> 1) -> Fraction(48, 1), Map(p2 -> 4, p1 -> 1, p4 -> 6, p3 -> 17) -> Fraction(-2112, 1), Map(p2 -> 8, p1 -> 1, p4 -> 7, p3 -> 13) -> Fraction(-18, 1), Map(p2 -> 12, p1 -> 1, p4 -> 14, p3 -> 1) -> Fraction(-1, 1), Map(p2 -> 4, p4 -> 3, p3 -> 21) -> Fraction(-286, 1), Map(p4 -> 8, p3 -> 10, p2 -> 8, p1 -> 1, p6 -> 1) -> Fraction(-25, 1), Map(p4 -> 3, p5 -> 1, p3 -> 19, p2 -> 5, p1 -> 1) -> Fraction(42, 1), Map(p2 -> 10, p1 -> 1, p4 -> 12, p3 -> 5) -> Fraction(-79, 1), Map(p4 -> 5, p5 -> 1, p3 -> 15, p2 -> 7, p1 -> 1) -> Fraction(63, 1), Map(p4 -> 5, p5 -> 3, p3 -> 13, p2 -> 6, p1 -> 1) -> Fraction(-28, 1), Map(p4 -> 4, p5 -> 1, p3 -> 17, p2 -> 6, p1 -> 1) -> Fraction(-63, 1), Map(p4 -> 3, p3 -> 20, p2 -> 3, p1 -> 1, p6 -> 1) -> Fraction(69, 1), Map(p4 -> 8, p5 -> 1, p3 -> 8, p2 -> 9, p1 -> 1, p6 -> 1) -> Fraction(25, 1), Map(p4 -> 7, p5 -> 1, p3 -> 10, p2 -> 8, p1 -> 1, p6 -> 1) -> Fraction(-92, 1), Map(p4 -> 2, p5 -> 3, p3 -> 19, p2 -> 3, p1 -> 1) -> Fraction(14, 1), Map(p2 -> 6, p1 -> 1, p4 -> 8, p3 -> 13) -> Fraction(-2141, 1), Map(p4 -> 3, p5 -> 1, p3 -> 21, p2 -> 2, p1 -> 1) -> Fraction(-227, 1), Map(p2 -> 3, p1 -> 1, p4 -> 5, p3 -> 19) -> Fraction(1418, 1)))
    val q = MultivariablePolynomial[Fraction[BigInt], String](Map(Map(p2 -> 6, p4 -> 6, p3 -> 14) -> Fraction(1716, 1), Map(p2 -> 4, p4 -> 4, p3 -> 18) -> Fraction(715, 1), Map(p2 -> 7, p4 -> 7, p3 -> 12) -> Fraction(-1716, 1), Map(p3 -> 26) -> Fraction(1, 1), Map(p2 -> 1, p4 -> 1, p3 -> 24) -> Fraction(-13, 1), Map(p2 -> 11, p4 -> 11, p3 -> 4) -> Fraction(-78, 1), Map(p2 -> 13, p4 -> 13) -> Fraction(-1, 1), Map(p2 -> 8, p4 -> 8, p3 -> 10) -> Fraction(1287, 1), Map(p2 -> 9, p4 -> 9, p3 -> 8) -> Fraction(-715, 1), Map(p2 -> 10, p4 -> 10, p3 -> 6) -> Fraction(286, 1), Map(p2 -> 2, p4 -> 2, p3 -> 22) -> Fraction(78, 1), Map(p2 -> 3, p4 -> 3, p3 -> 20) -> Fraction(-286, 1), Map(p2 -> 5, p4 -> 5, p3 -> 16) -> Fraction(-1287, 1), Map(p2 -> 12, p4 -> 12, p3 -> 2) -> Fraction(13, 1)))
    polynomials.gcd(p, q)
  }
  "rational functions with zero numerator" should "be zero" in {
    val polynomials = implicitly[MultivariablePolynomialAlgebraOverGCDRing[Fraction[Int], String]]
    val rationalFunctions = implicitly[Ring[Fraction[MultivariablePolynomial[Fraction[Int], String]]]]
    val m = polynomials.monomial(Map("d" -> 1), 1)
    val z = polynomials.zero

    Fraction(z, m) should equal(rationalFunctions.zero)
  }
}