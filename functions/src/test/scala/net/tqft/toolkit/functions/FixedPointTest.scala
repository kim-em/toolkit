package net.tqft.toolkit.functions

import org.scalatest._

class FixedPointTest extends FlatSpec with Matchers {
  
  "findRepeatingSubsequence" should "work correctly" in {
    import FixedPoint._
    
    ({ x: Int => (x + 1) % 7 }).findRepeatingSubsequence(-2) should equal(Seq(0,1,2,3,4,5,6), 2)
  }
  
}

