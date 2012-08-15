package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.math.BigInteger

@RunWith(classOf[JUnitRunner])
class NumberFieldTest extends FlatSpec with ShouldMatchers {

  "Number field arithmetic" should "be correct" in {
    import Implicits.integersAsRationals
    import AlgebraicNotation._

    val p: Polynomial[Fraction[Int]] = Polynomial(6 -> Fraction(1, 1), 4 -> Fraction(2, 1), 2 -> Fraction(-3, 1), 0 -> Fraction(-5, 1))
    val nf = NumberField(p)

    val q1: Polynomial[Fraction[Int]] = Polynomial(0 -> Fraction(-43, 125), 2 -> Fraction(6, 125), 4 -> Fraction(12, 125))

    nf.multiply(q1, q1) should equal(Polynomial(0 -> Fraction(1129, 15625), 2 -> Fraction(-228, 15625), 4 -> Fraction(-276, 15625)))
  }

  "Cyclotomic field arithmetic" should "be correct" in {
    implicit val rationals = Gadgets.Rationals
    val cyclotomicNumbers = NumberField.cyclotomic(6)(rationals)
    val zeta = Polynomial.identity(rationals)
    cyclotomicNumbers.multiplyByInt(cyclotomicNumbers.power(zeta, 1), 0) should equal(cyclotomicNumbers.zero)
    cyclotomicNumbers.multiplyByInt(cyclotomicNumbers.power(zeta, 0), 1) should not equal(cyclotomicNumbers.zero)
    cyclotomicNumbers.add(cyclotomicNumbers.multiplyByInt(cyclotomicNumbers.power(zeta, 0), 1), cyclotomicNumbers.multiplyByInt(cyclotomicNumbers.power(zeta, 1), 0))  should not equal(cyclotomicNumbers.zero)
  }
  
}
