package net.tqft.toolkit.collections

import net.tqft.toolkit.collections.RandomSample.iterable2RandomSampleable
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
 
@RunWith(classOf[JUnitRunner])
class RandomSampleTest extends FlatSpec with ShouldMatchers {

  "randomSample" should "not crash horribly" in {
    import RandomSample._
	  (1 to 10).randomSample(0).size should equal (0)
	  (1 to 10).randomSample(1).size should equal (10)
      (1 to 1000).randomSample(0.5).size < 950 should equal (true)
  }

	
}