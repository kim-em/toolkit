package net.tqft.toolkit.collections

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
 
@RunWith(classOf[JUnitRunner])
class RandomSampleTest extends FlatSpec with Matchers {

  "randomSample" should "not crash horribly" in {
    import RandomSample._
	  (1 to 10).randomSample(0).size should equal (0)
	  (1 to 10).randomSample(1).size should equal (10)
      (1 to 1000).randomSample(0.5).size < 950 should equal (true)
  }

	
}