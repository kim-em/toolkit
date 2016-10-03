package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups

@RunWith(classOf[JUnitRunner])
class FusionRingsTest extends FlatSpec with Matchers {

  "withObject" should "correctly find all fusion rings with a given object" in {
    FusionRings.withObject(FusionRings.Examples.AH1.structureCoefficients(1) /*, Some(FusionRings.Examples.AH1)*/ ).toSeq should have size (1)
    FusionRings.withObject(Seq(Seq(0, 1, 0, 0), Seq(1, 0, 1, 1), Seq(0, 1, 0, 0), Seq(0, 1, 0, 0))).toSeq should have size (1)
    FusionRings.withObject(Seq(Seq(0, 1, 0, 0, 0), Seq(1, 0, 1, 1, 1), Seq(0, 1, 0, 0, 0), Seq(0, 1, 0, 0, 0))).toSeq should have size (2)
  }

//  "representationOf" should "generate fusion rings from finite groups" in {
//    FusionRings.Examples.representationsOf(FiniteGroups.cyclicGroup(6)).globalDimensionLowerBound < 6.0 should be(true)
//    FusionRings.Examples.representationsOf(FiniteGroups.Mathieu11).globalDimensionLowerBound < FiniteGroups.Mathieu11.size should be(true)
//  }
}
