package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PolynomialsTest extends FlatSpec with Matchers {

  "Polynomial equality" should "be correct" in {
    SeqPolynomial(IndexedSeq(1, 0, 2)) should equal(Polynomial(0 -> 1, 1 -> 0, 2 -> 2))
  }

  "Polynomial arithmetic" should "be correct" in {
    import AlgebraicNotation._

    val p: Polynomial[Fraction[Int]] = Polynomial(2 -> Fraction(1, 1), 1 -> Fraction(3, 1), 0 -> Fraction(1, 1))
    val q: Polynomial[Fraction[Int]] = Polynomial(1 -> Fraction(1, 1), 0 -> Fraction(-2, 1))

    (p % q) should equal(Polynomial(0 -> Fraction(11, 1)))

    val a: Polynomial[Int] = Polynomial(3 -> 1, 2 -> -8, 1 -> 21, 0 -> -18)

    implicitly[Polynomials[Int]].evaluateAt(5)(a) should equal(12)

    val z: Polynomial[BigInt] = Polynomial(3 -> BigInt(1), 1 -> BigInt(16), 0 -> BigInt(-12), 2 -> BigInt(-7))
    val bigN = Fraction(BigInt(499), BigInt(100))
    implicitly[Polynomials[Fraction[BigInt]]].evaluateAt(bigN)(z: Polynomial[Fraction[BigInt]]) should equal(Fraction(BigInt(17790799), BigInt(1000000)))
  }
  "SeqPolynomial arithmetic" should "be correct" in {
    import AlgebraicNotation._

    val p: Polynomial[Fraction[Int]] = Polynomial(Seq(Fraction(1, 1), Fraction(3, 1), Fraction(1, 1)))
    val q: Polynomial[Fraction[Int]] = Polynomial(Seq(Fraction(-2, 1), Fraction(1, 1)))

    (p % q) should equal(Polynomial(Seq(Fraction(11, 1))))

    val a: Polynomial[Int] = Polynomial(Seq(-18, 21, -8, 1))

    implicitly[Polynomials[Int]].evaluateAt(5)(a) should equal(12)

    val z: Polynomial[BigInt] = Seq(-12, 16, -7, 1)
    val bigN = Fraction(BigInt(499), BigInt(100))
    implicitly[Polynomials[Fraction[BigInt]]].evaluateAt(bigN)(z: Polynomial[Fraction[BigInt]]) should equal(Fraction(BigInt(17790799), BigInt(1000000)))
  }

  "cyclotomic" should "give the cyclotomic polynomials" in {
    Polynomial.cyclotomic[Fraction[Int]](1) should equal(Polynomial(0 -> Fraction(-1, 1), 1 -> Fraction(1, 1)))
    Polynomial.cyclotomic[Fraction[Int]](2) should equal(Polynomial(0 -> Fraction(1, 1), 1 -> Fraction(1, 1)))
    Polynomial.cyclotomic[Fraction[Int]](3) should equal(Polynomial(0 -> Fraction(1, 1), 1 -> Fraction(1, 1), 2 -> Fraction(1, 1)))
    Polynomial.cyclotomic[Fraction[Int]](4) should equal(Polynomial(0 -> Fraction(1, 1), 2 -> Fraction(1, 1)))
    Polynomial.cyclotomic[Fraction[Int]](5) should equal(Polynomial(0 -> Fraction(1, 1), 1 -> Fraction(1, 1), 2 -> Fraction(1, 1), 3 -> Fraction(1, 1), 4 -> Fraction(1, 1)))
    Polynomial.cyclotomic[Fraction[Int]](6) should equal(Polynomial(0 -> Fraction(1, 1), 1 -> Fraction(-1, 1), 2 -> Fraction(1, 1)))
  }

  "subresultant_gcd" should "work correctly" in {
    val polynomials = implicitly[PolynomialsOverGCDRing[BigInt]]
    polynomials.subresultant_gcd(Map(8 -> 1, 6 -> 1, 4 -> -3, 3 -> -3, 2 -> 8, 1 -> 2, 0 -> -5), Map(6 -> 3, 4 -> 5, 2 -> -4, 1 -> -9, 0 -> 21)) should equal(Polynomial(Map(0 -> -260708)))
    polynomials.gcd(Map(8 -> 1, 6 -> 1, 4 -> -3, 3 -> -3, 2 -> 8, 1 -> 2, 0 -> -5), Map(6 -> 3, 4 -> 5, 2 -> -4, 1 -> -9, 0 -> 21)) should equal(Polynomial(Map(0 -> 1)))
    polynomials.gcd(Map(3 -> 1, 2 -> -1, 1 -> -1), Map(3 -> -3, 4 -> 1, 2 -> 3, 1 -> -1))
  }

  "resultant" should "work in an example" in {
    val polynomials = implicitly[PolynomialsOverGCDRing[BigInt]]
    val p0 = Map(0 -> 2, 1 -> -3, 1 -> 1)
    val p1 = Map(0 -> -60, 1 -> 47, 2 -> -12, 1 -> 1)
    polynomials.resultant(p0, p1) should equal(110)
  }

  "rational functions" should "multiply" in {
    val p = Fraction(Polynomial(Map(0 -> 1, 2 -> 1, 4 -> 1)), Polynomial(Map(2 -> -1)))
    val q = Fraction(Polynomial(Map(0 -> 1, 2 -> 1, 4 -> 1)), Polynomial(Map(2 -> -1)))
    val ring = implicitly[Field[Fraction[Polynomial[Int]]]]
    ring.multiply(p, q) should equal(Fraction(Polynomial(Map(0 -> 1, 2 -> 2, 4 -> 3, 6 -> 2, 8 -> 1)), Polynomial(Map(4 -> 1))))
  }

  "Sturm sequences" should "be calculated correctly" in {
    val p: Polynomial[Fraction[Int]] = Polynomial(4 -> 1, 3 -> 1, 1 -> -1, 0 -> -1)
    implicitly[PolynomialsOverField[Fraction[Int]]].sturmSequence(p) should equal(
      List[Polynomial[Fraction[Int]]](
        Polynomial(4 -> 1, 3 -> 1, 1 -> -1, 0 -> -1),
        Polynomial(3 -> 4, 2 -> 3, 0 -> -1),
        Polynomial(2 -> Fraction(3, 16), 1 -> Fraction(3, 4), 0 -> Fraction(15, 16)),
        Polynomial(1 -> -32, 0 -> -64),
        Polynomial(0 -> Fraction(-3, 16))))
  }
}