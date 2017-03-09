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

  "PlanarGraphEnumeration" should "say the children of the pentagon are the pentaforks" in {

    val pentaforks = Seq.tabulate(5)(i => PlanarGraph.spider.rotate(PlanarGraph.pentafork, -i))
    
    val context = PlanarGraphEnumerationContext(Seq(VertexType(3, 0, 1)))
    val root = context.PlanarGraphEnumeration(PlanarGraph.polygon(5))

    labels2(root.children) should equal(labels1(pentaforks))
  }

}