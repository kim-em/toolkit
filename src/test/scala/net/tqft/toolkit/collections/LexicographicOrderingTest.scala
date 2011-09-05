package net.tqft.toolkit.collections

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
 
@RunWith(classOf[JUnitRunner])
class LexicographicOrderingTest extends FlatSpec with ShouldMatchers {

  import LexicographicOrdering._
	
  "A List[Int]" should "implicitly also be an Ordered[List[Int]]" in {
	  List(1, 2, 3) < List(1, 3, 2) should be (true)
	  List(1, 2, 3, 4) < List(1, 2, 3) should be (false)
  }

	
}