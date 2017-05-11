package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.graphs.Dreadnaut

@RunWith(classOf[JUnitRunner])
class BoundaryConnectedPlanarGraphsTest extends FlatSpec with Matchers with IsomorphismMatchers {
  "BoundaryConnectedPlanarGraphs" should "count correctly" in {
    BoundaryConnectedPlanarGraphs(6, 1, Seq(PlanarGraph.polygon(4), PlanarGraph.polygon(5))).size should equal(1)
  }
  "BoundaryConnectedPlanarGraphs" should "count correctly (2)" in {
    BoundaryConnectedPlanarGraphs(8, 2, Seq(PlanarGraph.polygon(4), PlanarGraph.polygon(5))).size should equal(4)
  }
  "trivalent" should "count correctly" in {
    BoundaryConnectedPlanarGraphs.trivalent(8, 2).size should equal(820)
  }
  for (g <- ConnectedPlanarTrivalentGraphs(8, 2); if !g.hasTinyFace) DrawPlanarGraph.showPDF(g)
}