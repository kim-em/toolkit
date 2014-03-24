package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IntegersTest extends FlatSpec with ShouldMatchers {
  
  "Integer arithmetic" should "be correct" in {	  
	  Integers.gcd(15, 27) should equal(3)
      
      Integers.gcd(-26268, 5) should equal(1)
      Integers.gcd(-5, 1) should equal(1)
      Integers.gcd(5, -1) should equal(-1)
  }
  "BigInt arithmetic" should "be correct" in {	  
      BigIntegers.gcd(15, 27) should equal(3)
      
      BigIntegers.gcd(-26268, 5) should equal(1)
      BigIntegers.gcd(-5, 1) should equal(1)
      BigIntegers.gcd(5, -1) should equal(-1)
  }
  
}
