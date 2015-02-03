package net.tqft.toolkit.algebra.fusion2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PartialFusionRingTest extends FlatSpec with Matchers {

  "children" should "produce some offspring" in {
	  PartialFusionRingEnumeration(4,0,12.0).root.children should be ('nonEmpty)
  }
}