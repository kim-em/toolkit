package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.algebra.matrices.Matrices
import net.tqft.toolkit.algebra.grouptheory.FiniteGroup

@RunWith(classOf[JUnitRunner])
class PartialFusionRingTest extends FlatSpec with ShouldMatchers {

  {
    import net.tqft.toolkit.functions.Memo._
    implicit val toPartialFusionRing = { group: FiniteGroup[_] =>
      FusionRings.Examples.representationsOf(group): PartialFusionRing
    }.memo

    "verifyAncestry" should "work on cyclic groups" in {
      for (n <- 6 to 6) {
        val pfr: PartialFusionRing = FiniteGroups.cyclicGroup(n)
        println(pfr)
        println(pfr.ring.basis)
        pfr.verifyAncestry should be(true)
      }
    }
    //    "verifyAncestry" should "work on symmetric groups" in {
    //      for (n <- 2 to 2) {
    //        FiniteGroups.symmetricGroup(n).verifyAncestry should be(true)
    //      }
    //    }

    //    "progenitor" should "always be A1" in {
    //      for (n <- 4 to 7) {
    //         FiniteGroups.cyclicGroup(n).progenitor.ring.rank should be(1)
    //      }
    //    }
  }
}
