package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IntegersTest extends FlatSpec with ShouldMatchers {
  
  "Integer arithmetic" should "be correct" in {	  
      import Implicits.Integers
	  import AlgebraicNotation._
	  
	  Integers.gcd(15, 27) should equal(3)
      
      Integers.gcd(-26268, 5) should equal(1)
      Integers.gcd(-5, 1) should equal(1)
      Integers.gcd(5, -1) should equal(-1)
  }
  "BigInt arithmetic" should "be correct" in {	  
      import Implicits.BigIntegers
	  
	  BigIntegers.gcd(15, 27) should equal(3)
      
      BigIntegers.gcd(-26268, 5) should equal(1)
      BigIntegers.gcd(-5, 1) should equal(1)
      BigIntegers.gcd(5, -1) should equal(-1)
  }
  
  "sumOfSquareDecomposition" should "find all ways to write a positive integer as a sum of squares" in {
    import Gadgets.Integers
    Integers.sumOfSquaresDecomposition(5).size should equal (2)
  }
}
