package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PlanarGraphDanglinessTest extends FlatSpec with Matchers with IsomorphismMatchers {
  "dangliness" should "count correctly" in {
    PlanarGraph.polygon(4).dangliness(1) should equal (IndexedSeq(0,1,1,1,1))
    PlanarGraph.polygon(4).dangliness(2) should equal (IndexedSeq(0,2,2,2,2))
    PlanarGraph.spider.multiply(PlanarGraph.I, PlanarGraph.I, 1).dangliness(1) should equal (IndexedSeq(0,2,1,1,2))
    PlanarGraph.spider.multiply(PlanarGraph.I, PlanarGraph.I, 1).dangliness(2) should equal (IndexedSeq(0,1,3,3,1))
    PlanarGraph.spider.multiply(PlanarGraph.I, PlanarGraph.I, 1).dangliness(3) should equal (IndexedSeq(0,3,4,4,3))
  }
}