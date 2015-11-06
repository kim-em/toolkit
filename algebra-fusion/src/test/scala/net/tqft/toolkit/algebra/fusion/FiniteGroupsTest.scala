package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups

@RunWith(classOf[JUnitRunner])
class FiniteGroupsTest extends FlatSpec with Matchers {

  "fromFiniteGroup" should "construct from Z/2 a PartialFusionRing with valid ancestry" in {
    val pfr = PartialFusionRingEnumeration.fromFiniteGroup(FiniteGroups.cyclicGroup(2))
    pfr.verifyAncestry should be (true)
  }
  "fromFiniteGroup" should "construct from Z/3 a PartialFusionRing with valid ancestry" in {
    val pfr = PartialFusionRingEnumeration.fromFiniteGroup(FiniteGroups.cyclicGroup(3))
    pfr.verifyAncestry should be (true)
  }
  "fromFiniteGroup" should "construct from Z/2 x Z/2 a PartialFusionRing with valid ancestry" in {
    val pfr = PartialFusionRingEnumeration.fromFiniteGroup(FiniteGroups.power(FiniteGroups.cyclicGroup(2), 2))
    println(pfr)
    pfr.verifyStrictAncestryForSomeIsomorph should be (true)
  }
}