package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.graphs.Dreadnaut

@RunWith(classOf[JUnitRunner])
class PlanarGraphEnumerationTest extends FlatSpec with Matchers with IsomorphismMatchers {
  import Ordering.Implicits._
  def label(g: PlanarGraph) = g.canonicalFormWithDefect._1
  def labels1(gs: Seq[PlanarGraph]) = {
    gs.map(label).sorted
  }
  def labels2(gs: Seq[PlanarGraphEnumerationContext#PlanarGraphEnumeration]) = gs.map(g => label(g.G)).sorted

  val dpg = DrawPlanarGraph.withOutputPath("/Users/emilypeters/Documents/scratch/graphs")

  //  "PlanarGraphEnumeration" should "say the children of the pentagon are the pentaforks" in {
  //
  //    val pentaforks = Seq.tabulate(6)(i => PlanarGraph.spider.rotate(PlanarGraph.pentafork, i))
  //
  //    val context = PlanarGraphEnumerationContext(Seq(VertexType(3, 0, 1)))
  //    val root = context.PlanarGraphEnumeration(PlanarGraph.polygon(5))
  //
  //    val children = root.children
  //    //    for (c <- children) {
  //    //      DrawPlanarGraph.showPDF(c.G)
  //    //    }
  //
  //    labels2(children) should equal(labels1(pentaforks))
  //  }
  //  "PlanarGraphEnumeration" should "say the children of the hexagon are the hexaforks" in {
  //
  //    val hexaforks = Seq.tabulate(7)(i => PlanarGraph.spider.rotate(PlanarGraph.hexafork, i))
  //
  //    val context = PlanarGraphEnumerationContext(Seq(VertexType(3, 0, 1)))
  //    val root = context.PlanarGraphEnumeration(PlanarGraph.polygon(6))
  //
  //    val children = root.children
  //    //    for (c <- children) {
  //    //      DrawPlanarGraph.showPDF(c.G)
  //    //    }
  //
  //    labels2(children) should equal(labels1(hexaforks))
  //  }
//  "PlanarGraphEnumeration" should "find the children of a H" in {
//    dpg.showPDF(PlanarGraph.H)
//    val context = PlanarGraphEnumerationContext(Seq(VertexType(3, 0, 1)))
//    val root = context.PlanarGraphEnumeration(PlanarGraph.H)
//
//    val children = root.children
//    for (c <- children) {
//      dpg.showPDF(c.G)
//    }
//    // TODO think about what to test here? 
//  }
    "PlanarGraphEnumeration" should "find the children of a I" in {
      dpg.showPDF(PlanarGraph.I)

      val context = PlanarGraphEnumerationContext(Seq(VertexType(3, 0, 1)))
      val root = context.PlanarGraphEnumeration(PlanarGraph.I)
  
      val children = root.children
      for (c <- children) {
        dpg.showPDF(c.G)
      }
      // TODO think about what to test here? 
    }
  //  "PlanarGraphEnumeration" should "find the children of a pentafork" in {
  //
  //    val context = PlanarGraphEnumerationContext(Seq(VertexType(3, 0, 1)))
  //    val root = context.PlanarGraphEnumeration(PlanarGraph.pentafork)
  //
  //    val children = root.children
  //    //    for (c <- children) {
  //    //      DrawPlanarGraph.showPDF(c.G)
  //    //    }
  //    // TODO think about what to test here? 
  //  }
  //  "PlanarGraphEnumeration" should "find the children of the pentaforks" in {
  //    val pentaforks = Seq.tabulate(6)(i => PlanarGraph.spider.rotate(PlanarGraph.pentafork, i))
  //    val context = PlanarGraphEnumerationContext(Seq(VertexType(3, 0, 1)))
  //    val allChildren = for (p <- pentaforks) yield {
  //      val root = context.PlanarGraphEnumeration(p)
  //
  //      val children = root.children
  //      //          for (c <- children) {
  //      //            DrawPlanarGraph.showPDF(c.G)
  //      //          }
  //      children
  //    }
  //    (labels2(allChildren.flatten).size, labels2(allChildren.flatten).distinct.size) should equal((28, 28))
  //  }

}