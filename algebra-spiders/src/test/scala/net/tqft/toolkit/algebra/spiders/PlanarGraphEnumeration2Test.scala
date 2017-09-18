package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.graphs.Dreadnaut

@RunWith(classOf[JUnitRunner])
class PlanarGraphEnumeration2Test extends FlatSpec with Matchers with IsomorphismMatchers {
  def label(g: PlanarGraph) = g.canonicalFormWithDefect._1
  def labels1(gs: Seq[PlanarGraph]) = {
    gs.map(label).sorted
  }
  val spider = implicitly[DiagramSpider[PlanarGraph]]

  val dpg = DrawPlanarGraph.withOutputPath("/Users/scott/scratch/graphs")
  val context = PlanarGraphEnumerationContext2(Seq(VertexType(3, 0, 1)), Seq.empty, None, None)

  "graphs" should "be children of their parent" in {
    context.verify_child_of_parent(PlanarGraph.polygon(2)) should be(true)
    context.verify_child_of_parent(PlanarGraph.polygon(3)) should be(true)
    context.verify_child_of_parent(PlanarGraph.polygon(4)) should be(true)
    context.verify_child_of_parent(PlanarGraph.polygon(5)) should be(true)

    context.verify_child_of_parent(PlanarGraph.pentafork) should be(true)
    context.verify_child_of_parent(PlanarGraph.hexafork) should be(true)
    context.verify_child_of_parent(PlanarGraph.twoSquares) should be(true)
    context.verify_child_of_parent(PlanarGraph.pentaSquare) should be(true)
    for (i <- 0 until 3) {
      context.verify_ancestry(spider.rotate(PlanarGraph.pentapent, i)) should be(true)
    }
  }
}