package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class MultivariablePolynomialsTest extends FlatSpec with Matchers {

  //  "gcd of zero and a monomial " should "be that monomial" in {
  //    val polynomials = implicitly[MultivariablePolynomialAlgebraOverEuclideanRing[Fraction[Int], String]]
  //    val m = polynomials.monomial(Map("d" -> 1), 1)
  //    val z = polynomials.zero
  //    polynomials.gcd(m, z) should equal(m)
  //    polynomials.gcd(z, m) should equal(m)
  //  }
  //  "gcd of 0 and x" should "be x" in {
  //    val polynomials = implicitly[MultivariablePolynomialAlgebraOverEuclideanRing[Fraction[Int], String]]
  //    val t = "t"
  //    val b = "b"
  //    val d = "d"
  //    val p1 = MultivariablePolynomial(Map(Map(t -> 1, d -> 1) -> Fraction(1, 1)))
  //    val p2 = MultivariablePolynomial(Map(Map(t -> 2, d -> 3) -> Fraction(1, 1), Map(b -> 2, d -> 2) -> Fraction(2, 1), Map(t -> 2, d -> 1) -> Fraction(-1, 1), Map(b -> 2, d -> 3) -> Fraction(-1, 1), Map(t -> 1, b -> 1, d -> 1) -> Fraction(2, 1)))
  //    polynomials.gcd(polynomials.zero, p1) should equal(p1)
  //    polynomials.gcd(p1, polynomials.zero) should equal(p1)
  //    polynomials.gcd(polynomials.zero, p2) should equal(p2)
  //    polynomials.gcd(p2, polynomials.zero) should equal(p2)
  //  }
  "gcd" should "not overflow badly" in {
    type C = Int
    
    val polynomials = implicitly[MultivariablePolynomialAlgebraOverEuclideanRing[C, String]]
    val b = "b"
    val d = "d"
    val p = MultivariablePolynomial[C, String](Map(Map(d -> 5) -> -4, Map(d -> 1) -> -2, Map() -> -1, Map(d -> 7) -> 1, Map(d -> 3) -> 5, Map(d -> 4) -> -1, Map(d -> 2) -> 2))
    val q = MultivariablePolynomial[C, String](Map(Map(b -> 4, d -> 2) -> 2, Map(b -> 4, d -> 3) -> -3, Map(b -> 4, d -> 6) -> 3, Map(b -> 4, d -> 5) -> 5, Map(b -> 4, d -> 7) -> -4, Map(b -> 4, d -> 1) -> 1, Map(b -> 4, d -> 4) -> -5, Map(b -> 4, d -> 8) -> 1))
    val g = polynomials.gcd(p, q)
    g should equal(MultivariablePolynomial[C, String](Map(Map(d -> 3) -> -1, Map(d -> 2) -> 2, Map() -> -1)))
  }
  "rational functions with zero numerator" should "be zero" in {
    val polynomials = implicitly[MultivariablePolynomialAlgebraOverEuclideanRing[Fraction[Int], String]]
    val rationalFunctions = implicitly[Ring[Fraction[MultivariablePolynomial[Fraction[Int], String]]]]
    val m = polynomials.monomial(Map("d" -> 1), 1)
    val z = polynomials.zero

    Fraction(z, m) should equal(rationalFunctions.zero)
  }
}