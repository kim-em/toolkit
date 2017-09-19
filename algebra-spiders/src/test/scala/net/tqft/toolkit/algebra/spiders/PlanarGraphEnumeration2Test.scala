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

  val dpg = DrawPlanarGraph.withOutputPath("/Users/emilypeters/Documents/scratch/graphs")
  val context = PlanarGraphEnumerationContext2(Seq(VertexType(3, 0, 1)), Seq.empty, None, None)

  "graphs" should "be children of their parent" in {
//    context.verify_child_of_parent(PlanarGraph.polygon(2)) should be(true)
//    context.verify_child_of_parent(PlanarGraph.polygon(3)) should be(true)
//    context.verify_child_of_parent(PlanarGraph.polygon(4)) should be(true)
//    context.verify_child_of_parent(PlanarGraph.polygon(5)) should be(true)
//
//    for (i <- 0 until 6) {
//      context.verify_ancestry(spider.rotate(PlanarGraph.pentafork, i)) should be(true)
//    }
//    for (i <- 0 until 7) {
//      context.verify_ancestry(spider.rotate(PlanarGraph.hexafork, i)) should be(true)
//    }
//    for (i <- 0 until 2) {
//      context.verify_ancestry(spider.rotate(PlanarGraph.twoSquares, i)) should be(true)
//    }
    
    val p = spider.rotate(PlanarGraph.pentaSquare, 2)
    val p1 =  context.parent(p)
    dpg.showPDF(p1)
    val p2 =  context.parent(p1)
    dpg.showPDF(p2)
//    val p3 =  context.parent(context.parent(context.parent(p)))
//    dpg.showPDF(p3)
//    val p4 =  context.parent(context.parent(context.parent(context.parent(p))))
//    dpg.showPDF(p4)
//    val p5 =  context.parent(context.parent(context.parent(context.parent(context.parent(p)))))
//    dpg.showPDF(p5)
    
//    context.verify_ancestry(context.parent(context.parent(context.parent(context.parent(context.parent(context.parent(p))))))) should be (true)
//    context.verify_ancestry(p) should be(true)
//    
//    for (i <- 0 until 5) {
//      context.verify_ancestry(spider.rotate(PlanarGraph.pentaSquare, i)) should be(true)
//    }
//    for (i <- 0 until 3) {
//      context.verify_ancestry(spider.rotate(PlanarGraph.pentapent, i)) should be(true)
//    }
  }
}