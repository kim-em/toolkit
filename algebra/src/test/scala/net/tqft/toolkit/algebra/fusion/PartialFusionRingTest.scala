package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups

@RunWith(classOf[JUnitRunner])
class PartialFusionRingTest extends FlatSpec with ShouldMatchers {

  //  "lowerObjects" should "not be empty" in {
  //    val pfr = ???
  //    println("pfr.depth -> " + pfr.depth)
  //    println("pfr.depths -> " + pfr.depths)
  //    println("pfr.ring.duality -> " + pfr.ring.duality)
  //    pfr.lowerObjects.elements should not be ('empty)
  //  }

  "children" should "all be singly generated" in {
    val pfr = PartialFusionRing(2, FusionRing(Vector(Matrix(5, IndexedSeq(IndexedSeq(1, 0, 0, 0, 0), IndexedSeq(0, 1, 0, 0, 0), IndexedSeq(0, 0, 1, 0, 0), IndexedSeq(0, 0, 0, 1, 0), IndexedSeq(0, 0, 0, 0, 1))), Matrix(5, IndexedSeq(IndexedSeq(0, 1, 0, 0, 0), IndexedSeq(0, 0, 0, 1, 0), IndexedSeq(0, 0, 0, 0, 1), IndexedSeq(0, 0, 1, 0, 0), IndexedSeq(1, 0, 0, 0, 0))), Matrix(5, IndexedSeq(IndexedSeq(0, 0, 1, 0, 0), IndexedSeq(0, 0, 0, 0, 1), IndexedSeq(0, 1, 0, 0, 0), IndexedSeq(1, 0, 0, 0, 0), IndexedSeq(0, 0, 0, 1, 0))), Matrix(5, IndexedSeq(IndexedSeq(0, 0, 0, 1, 0), IndexedSeq(0, 0, 1, 0, 0), IndexedSeq(1, 0, 0, 0, 0), IndexedSeq(0, 0, 0, 0, 1), IndexedSeq(0, 1, 0, 0, 0))), Matrix(5, IndexedSeq(IndexedSeq(0, 0, 0, 0, 1), IndexedSeq(1, 0, 0, 0, 0), IndexedSeq(0, 0, 0, 1, 0), IndexedSeq(0, 1, 0, 0, 0), IndexedSeq(0, 0, 1, 0, 0))))), 6.0)
    pfr.children should have size (1)
  }

  {
    val Z5 = FusionRings.Examples.representationsOf(FiniteGroups.cyclicGroup(5))
    val pfr = PartialFusionRing(3, Z5, 6.0)

    "verifyAncestry" should "work on Z/5" in {
      pfr.verifyAncestry should be(true)
    }

    "progenitor" should "always be A1" in {
      pfr.progenitor.ring.rank should be(1)
    }
  }
}
