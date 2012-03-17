package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.math.BigInteger

@RunWith(classOf[JUnitRunner])
class RationalsTest extends FlatSpec with ShouldMatchers {
  
  "Rational arithmetic" should "be correct" in {	  
      import Implicits.Integers
      import Implicits.Rationals
      import Implicits.integersAsRationals
	  import AlgebraicNotation._
	  
	  val x: Fraction[Int] = Fraction(3, 10)
	  val y: Fraction[Int] = Fraction(2, 3)
	  
	  x * 5 should equal(Fraction(3, 2))      
      x * y should equal(Fraction(1, 5))
      
      Fraction(0, -1) should equal(Fraction(0, 1))
      Fraction(-1, 1) should equal(Fraction(1, -1))
  }
  
  "BigRational arithmetic" should "be correct" in {	  
      import Implicits.BigIntegers
      import Implicits.BigRationals
      import Implicits.integersAsBigInts
      import Implicits.bigIntegersAsBigRationals
	  import AlgebraicNotation._
	  
	  val x: Fraction[BigInt] = Fraction(3, 10)
	  val y: Fraction[BigInt] = Fraction(2, 3)
	  
	  x * Fraction(5, 1) should equal(Fraction[BigInt](3, 2))      
      x * y should equal(Fraction[BigInt](1, 5))

      Fraction[BigInt](0, -1) should equal(Fraction[BigInt](0, 1))
      Fraction[BigInt](-1, 1) should equal(Fraction[BigInt](1, -1))
      Fraction[BigInt](3, -7).denominator should equal(BigInt(7))
      Fraction[BigInt](3, -7).numerator should equal(BigInt(-3))
  }
}
