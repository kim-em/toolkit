package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class TrivalentEnumerationTest extends FlatSpec with Matchers with IsomorphismMatchers {

  "BoundaryConnectedPlanarGraphs.trivalent" should "find 1 graph with 6 boundary points and 1 internal face" in {
    BoundaryConnectedPlanarGraphs.trivalent(6, 1).size should equal(1)
  }

  "BoundaryConnectedPlanarGraphs.trivalent" should "find 6 graphs with 5 boundary points and 1 internal face" in {
    BoundaryConnectedPlanarGraphs.trivalent(5, 1).size should equal(6)
  }

  "BoundaryConnectedPlanarGraphs.trivalent" should "find 4 graphs with 4 boundary points and no internal faces" in {
    BoundaryConnectedPlanarGraphs.trivalent(4, 0).size should equal(4)
  }

  "BoundaryConnectedPlanarGraphs.trivalent" should "find 1 graph with 4 boundary points and 1 internal face" in {
    BoundaryConnectedPlanarGraphs.trivalent(4, 1).size should equal(1)
  }

  "BoundaryConnectedPlanarGraphs.trivalent" should "find no graphs with 3 boundary points and 1 internal face" in {
    BoundaryConnectedPlanarGraphs.trivalent(3, 1).size should equal(0)
  }

  "BoundaryConnectedPlanarGraphs.trivalent" should "find 1 graph with 3 boundary points and no internal faces" in {
    BoundaryConnectedPlanarGraphs.trivalent(3, 0).size should equal(1)
  }
}