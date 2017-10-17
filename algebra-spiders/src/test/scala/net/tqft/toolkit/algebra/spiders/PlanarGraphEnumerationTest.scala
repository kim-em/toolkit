package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.graphs.Dreadnaut

@RunWith(classOf[JUnitRunner])
class PlanarGraphEnumerationTest extends FlatSpec with Matchers with IsomorphismMatchers {
  def label(g: PlanarGraph) = g.canonicalFormWithDefect._1
  def labels1(gs: Seq[PlanarGraph]) = {
    gs.map(label).sorted
  }
  val spider = implicitly[DiagramSpider[PlanarGraph]]

  val dpg = {
    System.getProperty("user.name") match {
      case "scott"       => DrawPlanarGraph.withOutputPath("/Users/scott/scratch/graphs")
      case "emilypeters" => DrawPlanarGraph.withOutputPath("/Users/emilypeters/Documents/scratch/graphs")
    }
  }

  "graphs" should "be children of their parent" in {
    val context = PlanarGraphEnumerationContext(Seq(VertexType(3, 0, 1)), Seq.empty, Seq(PlanarGraph.star(3)), 20, 5)

    context.verify_ancestry(PlanarGraph.polygon(2)) should be(true)
    context.verify_ancestry(PlanarGraph.polygon(3)) should be(true)
    context.verify_ancestry(PlanarGraph.polygon(4)) should be(true)
    context.verify_ancestry(PlanarGraph.polygon(5)) should be(true)

    for (i <- 0 until 6) {
      context.verify_ancestry(spider.rotate(PlanarGraph.pentafork, i)) should be(true)
    }
    for (i <- 0 until 7) {
      context.verify_ancestry(spider.rotate(PlanarGraph.hexafork, i)) should be(true)
    }
    for (i <- 0 until 2) {
      context.verify_ancestry(spider.rotate(PlanarGraph.twoSquares, i)) should be(true)
    }

    for (i <- 0 until 5) {
      context.verify_ancestry(spider.rotate(PlanarGraph.pentaSquare, i)) should be(true)
    }
    for (i <- 0 until 3) {
      context.verify_ancestry(spider.rotate(PlanarGraph.pentapent, i)) should be(true)
    }
  }
  "we should find all the trivalent graphs without small faces" should "" in {
    val maxFaces = 3
    val maxBoundaryPoints = 6 // we've tested up to 7
    val context = PlanarGraphEnumerationContext(Seq(VertexType(3, 0, 1)), Seq.tabulate(4)(i => PlanarGraph.polygon(i + 1)), Seq(PlanarGraph.star(3)), maxFaces, maxBoundaryPoints)
    val root = PlanarGraph.star(3)
 
    val descendants = context.connectedGraphs.toStream
    val maxVertices = descendants.map(_.numberOfInternalVertices).max

    val counts = for (b <- 3 to maxBoundaryPoints) yield {
      for (v <- 1 to maxVertices) yield {
        descendants.count(g => g.numberOfBoundaryPoints == b && g.numberOfInternalVertices == v)
      }
    }

    val expectedCounts = Seq(
      Vector(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 5, 0, 1, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 14, 0, 7, 0, 3, 0, 2, 0),
      Vector(0, 0, 0, 0, 42, 0, 36, 0, 28, 0, 28)).take(maxBoundaryPoints - 2).map(_.take(maxVertices))

    counts should equal(expectedCounts)
  }
  "we should find all the tetravalent graphs without bigons" should "" in {
    val maxFaces = 2 
    val maxBoundaryPoints = 8 // we've tested up to 8
    val root = PlanarGraph.star(4)
    val context = PlanarGraphEnumerationContext(Seq(VertexType(4, 0, 1)), Seq(spider.multiply(root, root, 2)), Seq(root), maxFaces, maxBoundaryPoints)

    val descendants = context.connectedGraphs.toStream
    val maxVertices = descendants.map(_.numberOfInternalVertices).max

    val counts = for (b <- 3 to maxBoundaryPoints) yield {
      for (v <- 1 to maxVertices) yield {
        descendants.count(g => g.numberOfBoundaryPoints == b && g.numberOfInternalVertices == v)
      }
    }

    val expectedCounts = Seq(
      Vector(0, 0, 0, 0, 0, 0),
      Vector(1, 0, 0, 0, 1, 4),
      Vector(0, 0, 0, 0, 0, 0),
      Vector(0, 3, 2, 3, 6, 20),
      Vector(0, 0, 0, 0, 0, 0),
      Vector(0, 0, 12, 18, 36, 84)).take(maxBoundaryPoints - 2).map(_.take(maxVertices))

    counts should equal(expectedCounts)
  }
  "we should find all the tetravalent graphs without small faces" should "" in {
    val maxFaces = 3 
    val maxBoundaryPoints = 10 // we've tested up to 10
    val root = PlanarGraph.star(4)
    val bigon = spider.multiply(root, root, 2)
    val triangle = spider.multiply(spider.rotate(spider.multiply(root, root, 1), 2), root, 2)
    val context = PlanarGraphEnumerationContext(Seq(VertexType(4, 0, 1)), Seq(bigon, triangle), Seq(root), maxFaces, maxBoundaryPoints)

    val descendants = context.connectedGraphs.toStream
    val maxVertices = descendants.map(_.numberOfInternalVertices).max

    val counts = for (b <- 3 to maxBoundaryPoints) yield {
      for (v <- 1 to maxVertices) yield {
        descendants.count(g => g.numberOfBoundaryPoints == b && g.numberOfInternalVertices == v)
      }
    }

    // see https://tqft.net/web/notes/load.php?name=projects/enumerating-graphs/20171002-counting-tetravalent-graphs
    val expectedCounts = Seq(
      Vector(0, 0, 0, 0, 0, 0),
      Vector(1, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0),
      Vector(0, 3, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0),
      Vector(0, 0, 12, 2, 0, 0),
      Vector(0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 55, 22, 5)).take(maxBoundaryPoints - 2).map(_.take(maxVertices))

    counts should equal(expectedCounts)
  }

}