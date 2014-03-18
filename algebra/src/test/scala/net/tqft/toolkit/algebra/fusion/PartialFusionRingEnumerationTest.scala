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
class PartialFusionRingEnumerationTest extends FlatSpec with ShouldMatchers {

  "descendants" should "produce the right number of results" in {
    val seed = FusionRings.Examples.rank2(0)
    val generators = Set(1)
    PartialFusionRing(seed.depthWithRespectTo(generators).max + 1, generators, seed, 4.0).descendants().size should equal(3)
    PartialFusionRing(seed.depthWithRespectTo(generators).max + 1, generators, seed, 6.0).descendants().size should equal(11)
    PartialFusionRing(seed.depthWithRespectTo(generators).max + 1, generators, seed, 9.0).descendants().size should equal(41)
  }

}
