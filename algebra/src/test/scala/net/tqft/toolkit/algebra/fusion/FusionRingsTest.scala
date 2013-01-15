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

	
}
