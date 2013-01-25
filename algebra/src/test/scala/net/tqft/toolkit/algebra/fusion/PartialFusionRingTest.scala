package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups

@RunWith(classOf[JUnitRunner])
class PartialFusionRingTest extends FlatSpec with ShouldMatchers {
	
 "verifyAncestry" should "work on Z/5" in {
   val Z5 = FusionRings.Examples.representationsOf(FiniteGroups.cyclicGroup(5))
   val pfr = PartialFusionRing(3, Z5, 6.0)
   
//pfr.verifyAncestry(isomorphismQ)
 }
}
