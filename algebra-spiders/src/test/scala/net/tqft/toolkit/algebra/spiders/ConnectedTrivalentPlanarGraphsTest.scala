package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class ConnectedTrivalentPlanarGraphsTest extends FlatSpec with Matchers with IsomorphismMatchers {

  "ConnectedTrivalentPlanarGraphs" should "count the number of cubic graphs with 6 boundary points" in {
    ConnectedTrivalentPlanarGraphs(6, 1).size should equal (1) 
  }
  
}