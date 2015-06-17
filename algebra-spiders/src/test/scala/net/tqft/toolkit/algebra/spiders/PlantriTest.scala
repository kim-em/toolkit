package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PlantriTest extends FlatSpec with Matchers with IsomorphismMatchers {

  "Plantri" should "count the number of graphs with cubic graphs with 6 boundary points" in {
    Plantri.connectedTrivalentPlanarGraphs(6, 1).size should equal (1) 
  }
  
}