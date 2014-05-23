package net.tqft.toolkit.orderings

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
 
@RunWith(classOf[JUnitRunner])
class LexicographicOrderingTest extends FlatSpec with Matchers {

  "map ordering" should "work as follows" in {
    import LexicographicOrdering.mapOrdering
    val o = implicitly[Ordering[Map[Int, Int]]]
    o.compare(Map(0 -> 1, 1 -> 1), Map(1-> 0, 2-> 2)) < 0 should be(true)
    o.compare(Map(0 -> 1, 1 -> 1), Map(1-> 1, 2-> 2)) < 0 should be(true)
    o.compare(Map(0 -> 1, 1 -> 1), Map(1-> 2, 2-> 2)) < 0 should be(true)
    o.compare(Map(0 -> 1, 1 -> 3), Map(0-> 0, 2-> 2)) < 0 should be(true)
  }

	
}