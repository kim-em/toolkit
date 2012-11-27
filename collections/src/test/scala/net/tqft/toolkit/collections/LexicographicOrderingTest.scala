package net.tqft.toolkit.collections

import net.tqft.toolkit.collections.LexicographicOrdering.Seq2LexicographicOrderedSeq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
 
@RunWith(classOf[JUnitRunner])
class LexicographicOrderingTest extends FlatSpec with ShouldMatchers {

  import LexicographicOrdering._
	
  "A List[Int]" should "implicitly also be an Ordered[List[Int]]" in {
	  List(1, 2, 3) < List(1, 3, 2) should be (true)
	  List(1, 2, 3, 4) < List(1, 2, 3) should be (false)
  }

	
}