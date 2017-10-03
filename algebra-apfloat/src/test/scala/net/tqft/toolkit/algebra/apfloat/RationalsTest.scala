package net.tqft.toolkit.algebra.apfloat

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.algebra._
import org.apfloat.Apint

@RunWith(classOf[JUnitRunner])
class RationalsTest extends FlatSpec with Matchers {

  "Apint arithmetic" should "be correct" in {
    import AlgebraicNotation._
    import Gadgets._

    val x: Fraction[Apint] = Fraction(3, 10)
    val y: Fraction[Apint] = Fraction(2, 3)

    x * Fraction(5, 1) should equal(Fraction[Apint](3, 2))
    x * y should equal(Fraction[Apint](1, 5))

    Fraction[Apint](0, -1) should equal(Fraction[Apint](0, 1))
    Fraction[Apint](-1, 1) should equal(Fraction[Apint](1, -1))
    Fraction[Apint](3, -7).denominator should equal(new Apint(7))
    Fraction[Apint](3, -7).numerator should equal(new Apint(-3))
    Fraction[Apint](1, 1) + Fraction[Apint](-1, 1) should equal(Fraction[Apint](0, 1))

  }
}
