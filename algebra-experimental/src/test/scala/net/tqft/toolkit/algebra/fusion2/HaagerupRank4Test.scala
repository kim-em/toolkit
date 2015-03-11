package net.tqft.toolkit.algebra.fusion2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class HaagerupRank4Test extends FlatSpec with Matchers {

  "the rank 4 Haagerup category" should "have a valid ancestry" in {
    PartialFusionRingEnumeration(4, 0).PartialFusionRing("4,0 2 111121110121221111110111010 35.72").verifyAncestryForSomeIsomorph should be (true)
  }
}