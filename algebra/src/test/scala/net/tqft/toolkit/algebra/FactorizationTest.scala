package net.tqft.toolkit.algebra

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FactorizationTest extends FlatSpec with Matchers {
  
  "factorization" should "be correct" in {
    import ECMFactorization._
    
    (6).factor should equal(Map(2 -> 1, 3-> 1))    
  }
  
}
