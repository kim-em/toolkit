package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.matrices.Matrix

@RunWith(classOf[JUnitRunner])
class FusionRingsTest extends FlatSpec with ShouldMatchers {

  "withObject" should "correctly find all fusion rings with a given object" in {
    FusionRings.withObject(FusionRings.Examples.AH1.structureCoefficients(1) /*, Some(FusionRings.Examples.AH1)*/ ) should have size (1)
  }

  "relabel" should "do nothing, when the labelling is an automorphism" in {
    val ring = FusionRing(List[Matrix[Int]](
      List(List(1, 0, 0, 0), List(0, 1, 0, 0), List(0, 0, 1, 0), List(0, 0, 0, 1)),
      List(List(0, 1, 0, 0), List(1, 0, 1, 1), List(0, 1, 0, 0), List(0, 1, 0, 0)),
      List(List(0, 0, 1, 0), List(0, 1, 0, 0), List(1, 0, 0, 0), List(0, 0, 0, 0)),
      List(List(0, 0, 0, 1), List(0, 1, 0, 0), List(0, 0, 0, 0), List(1, 0, 0, 0))))
    ring.relabel(IndexedSeq(0, 1, 3, 2)) should equal(ring)
  }

  "canonicalRelabelling" should "not break associativity" in {
    FusionRings.Examples.AH1.canonicalRelabelling().verifyAssociativity should be(true)
  }
}
