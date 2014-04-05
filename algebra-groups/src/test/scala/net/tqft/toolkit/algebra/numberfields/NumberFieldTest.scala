package net.tqft.toolkit.algebra.numberfields

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials.Polynomial

@RunWith(classOf[JUnitRunner])
class NumberFieldTest extends FlatSpec with Matchers {

  "Number field arithmetic" should "be correct" in {
    val p: Polynomial[Fraction[Int]] = Polynomial(6 -> Fraction(1, 1), 4 -> Fraction(2, 1), 2 -> Fraction(-3, 1), 0 -> Fraction(-5, 1))
    val nf = NumberField(p)

    val q1: Polynomial[Fraction[Int]] = Polynomial(0 -> Fraction(-43, 125), 2 -> Fraction(6, 125), 4 -> Fraction(12, 125))

    nf.multiply(q1, q1) should equal(Polynomial(0 -> Fraction(1129, 15625), 2 -> Fraction(-228, 15625), 4 -> Fraction(-276, 15625)))
  }

  "Cyclotomic field arithmetic" should "be correct" in {
    val cyclotomicNumbers = NumberField.cyclotomic[Fraction[Int]](6)
    val zeta = Polynomial.identity[Fraction[Int]]
    cyclotomicNumbers.multiplyByInt(cyclotomicNumbers.power(zeta, 1), 0) should equal(cyclotomicNumbers.zero)
    cyclotomicNumbers.multiplyByInt(cyclotomicNumbers.power(zeta, 0), 1) should not equal(cyclotomicNumbers.zero)
    cyclotomicNumbers.add(cyclotomicNumbers.multiplyByInt(cyclotomicNumbers.power(zeta, 0), 1), cyclotomicNumbers.multiplyByInt(cyclotomicNumbers.power(zeta, 1), 0))  should not equal(cyclotomicNumbers.zero)
    cyclotomicNumbers.power(zeta, 6) should equal(cyclotomicNumbers.one)
  }
  
}
