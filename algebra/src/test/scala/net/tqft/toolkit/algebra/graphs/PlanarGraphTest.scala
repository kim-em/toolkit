package net.tqft.toolkit.algebra.graphs

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

@RunWith(classOf[JUnitRunner])
class PlanarGraphTest extends FlatSpec with ShouldMatchers {
  "PlanarGraph" should "do some computations" in {

    val spider = implicitly[Spider[PlanarGraph]]
    
    spider.canonicalForm(PlanarGraph.loop(2)) should equal(spider.canonicalForm(spider.stitch(PlanarGraph.strand)))
  }
}