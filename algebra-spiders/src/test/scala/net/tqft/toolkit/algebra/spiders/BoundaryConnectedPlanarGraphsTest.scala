package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.graphs.Dreadnaut

@RunWith(classOf[JUnitRunner])
class BoundaryConnectedPlanarGraphsTest extends FlatSpec with Matchers with IsomorphismMatchers {
  "trivalent" should "count correctly" in {
    BoundaryConnectedPlanarGraphs.trivalent(8, 2).size should equal(820)
  }

}

