package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class GraphsGeneratedByTest extends FlatSpec with Matchers with IsomorphismMatchers {
  val trivalentEnumerator = GraphsGeneratedBy(Seq((3, 0, 1)))
  val tetravalentEnumerator = GraphsGeneratedBy(Seq((4, 0, 1)))

  val freeTetravalent = tetravalentEnumerator.avoiding(Seq())
  val withoutSmallFaces = trivalentEnumerator.avoiding(for (i <- 1 to 4) yield PlanarGraph.polygon(i))
  val withoutTinyFaces = trivalentEnumerator.avoiding(for (i <- 1 to 3) yield PlanarGraph.polygon(i))

  "byNumberOfVertices" should "find 2 tetravalent diagram with 0 boundary points and 1 vertex" in {
    freeTetravalent.byNumberOfVertices(0, 1).size should equal(2)
  }
  "byNumberOfVertices" should "find 6 connected tetravalent diagram with 0 boundary points and 2 vertices" in {
    tetravalentEnumerator.avoiding(freeTetravalent.byNumberOfVertices(0, 1)).byNumberOfVertices(0, 2).size should equal(6)
  }
  "byNumberOfVertices" should "find 6 tetravalent diagram with 2 boundary points and 1 vertex" in {
    freeTetravalent.byNumberOfVertices(2, 1).size should equal(6)
  }
  "byNumberOfVertices" should "find 2 connected tetravalent diagram with 2 boundary points and 1 vertex" in {
    tetravalentEnumerator.avoiding(freeTetravalent.byNumberOfVertices(0, 1)).byNumberOfVertices(2, 1).size should equal(2)
  }
  "byNumberOfVertices" should "find 1 connected tetravalent diagram with 0 boundary points and 2 vertices and no twists" in {
    tetravalentEnumerator.avoiding(tetravalentEnumerator.avoiding(freeTetravalent.byNumberOfVertices(0, 1)).byNumberOfVertices(2, 1).take(1)).byNumberOfVertices(0, 2).size should equal(1)
  }

  val R1 = implicitly[Spider[PlanarGraph]].stitch(PlanarGraph.star(4, 1))
  val phi = implicitly[Spider[PlanarGraph]].multiply(PlanarGraph.star(4, 1), PlanarGraph.star(4, 1), 3)

  "byNumberOfVertices" should "find 4 tetravalent diagrams without twists with 2 boundary points and 3 vertices" in {
    val diagrams = tetravalentEnumerator.avoiding(Seq(R1, phi)).byNumberOfVertices(2, 3)
//    for(d <- diagrams) {
//      println(d)
//    }
    diagrams.size should equal(6)
  }

  val R2 = PlanarGraph(9, Vector(List((3, 9), (5, 13), (6, 11), (4, 12)), List((3, 13), (4, 9), (7, 12), (8, 10)), List((5, 11), (8, 13), (7, 10), (6, 12))), IndexedSeq((1,0), (1,0)), 0)
  val smallTetravalent = tetravalentEnumerator.avoiding(Seq(R1, R2))

  "byNumberOfVertices" should "find 5 small tetravalent diagrams with 6 boundary points and no vertices" in {
    smallTetravalent.byNumberOfVertices(6, 0).size should equal(5)
  }
  "byNumberOfVertices" should "find 6 small tetravalent diagrams with 6 boundary points and 1 vertices" in {
    smallTetravalent.byNumberOfVertices(6, 1).size should equal(6)
  }
  "byNumberOfVertices" should "find 3 small tetravalent diagrams with 6 boundary points and 2 vertices" in {
    smallTetravalent.byNumberOfVertices(6, 2).size should equal(3)
  }
  "byNumberOfVertices" should "find 1 diagrams with 0 boundary points and 0 vertices" in {
    withoutSmallFaces.byNumberOfVertices(0, 0).size should equal(1)
  }
  "byNumberOfVertices" should "find 1 diagrams with 2 boundary points and 0 vertices" in {
    withoutSmallFaces.byNumberOfVertices(2, 0).size should equal(1)
  }
  "byNumberOfVertices" should "find 1 diagrams with 3 boundary points and 1 vertices" in {
    withoutSmallFaces.byNumberOfVertices(3, 1).size should equal(1)
  }
  "byNumberOfVertices" should "find 2 diagrams with 4 boundary points and no vertices" in {
    withoutSmallFaces.byNumberOfVertices(4, 0).size should equal(2)
  }
  "byNumberOfVertices" should "find 4 diagrams with 4 boundary points and 2 vertices" in {
    withoutSmallFaces.byNumberOfVertices(4, 2).size should equal(2)
  }
  "byNumberOfVertices" should "find 0 diagrams with 4 boundary points and 4 vertices" in {
    withoutSmallFaces.byNumberOfVertices(4, 4).size should equal(0)
  }
  "withoutTinyFaces.byNumberOfVertices" should "find 1 diagrams with 4 boundary points and 4 vertices" in {
    withoutTinyFaces.byNumberOfVertices(4, 4).size should equal(1)
  }
  "withoutSmallFaces.byNumberOfVertices" should "find 5 diagrams with 5 boundary points and 1 vertices" in {
    withoutSmallFaces.byNumberOfVertices(5, 1).size should equal(5)
  }
  "withoutSmallFaces.byNumberOfVertices" should "find 5 diagrams with 5 boundary points and 3 vertices" in {
    withoutSmallFaces.byNumberOfVertices(5, 3).size should equal(5)
  }
  "withoutSmallFaces.byNumberOfVertices" should "find 1 diagrams with 5 boundary points and 5 vertices" in {
    withoutSmallFaces.byNumberOfVertices(5, 5).size should equal(1)
  }
  "withoutTinyFaces.byNumberOfVertices" should "find 1 diagrams with 5 boundary points and 5 vertices" in {
    withoutTinyFaces.byNumberOfVertices(5, 5).size should equal(6)
  }
  "withoutSmallFaces.byNumberOfVertices" should "find 5 diagrams with 6 boundary points and 0 vertices" in {
    withoutSmallFaces.byNumberOfVertices(6, 0).size should equal(5)
  }
  "withoutSmallFaces.byNumberOfVertices" should "find 15 diagrams with 6 boundary points and 2 vertices" in {
    withoutSmallFaces.byNumberOfVertices(6, 2).size should equal(15)
  }
  "withoutSmallFaces.byNumberOfVertices" should "find 14 diagrams with 6 boundary points and 4 vertices" in {
    withoutSmallFaces.byNumberOfVertices(6, 4).size should equal(14)
  }
  "withoutSmallFaces.byNumberOfVertices" should "find 7 diagrams with 6 boundary points and 6 vertices" in {
    withoutSmallFaces.byNumberOfVertices(6, 6).size should equal(7)
  }
  "withoutSmallFaces.byNumberOfVertices" should "find 3 diagrams with 6 boundary points and 8 vertices" in {
    withoutSmallFaces.byNumberOfVertices(6, 8).size should equal(3)
  }
  "withoutSmallFaces.byNumberOfVertices" should "find 2 diagrams with 6 boundary points and 10 vertices" in {
    withoutSmallFaces.byNumberOfVertices(6, 10).size should equal(2)
  }
  "withoutSmallFaces.byNumberOfVertices" should "find 3 diagrams with 6 boundary points and 12 vertices" in {
    withoutSmallFaces.byNumberOfVertices(6, 12).size should equal(3)
  }

  "byNumberOfFaces" should "find 4 diagrams in D[4,0]" in {
    withoutSmallFaces.byNumberOfFaces(4, 0).size should equal(4)
  }
  "byNumberOfFaces" should "find 10 diagrams in D[5,0]" in {
    withoutSmallFaces.byNumberOfFaces(5, 0).size should equal(10)
  }
  "byNumberOfFaces" should "find 34 diagrams in D[6,0]" in {
    withoutSmallFaces.byNumberOfFaces(6, 0).size should equal(34)
  }
  "byNumberOfFaces" should "find 41 diagrams in D[6,1]" in {
    withoutSmallFaces.byNumberOfFaces(6, 1).size should equal(41)
  }
  "byNumberOfFaces" should "find 44 diagrams in D[6,2]" in {
    withoutSmallFaces.byNumberOfFaces(6, 2).size should equal(44)
  }
  "byNumberOfFaces" should "find 46 diagrams in D[6,3]" in {
    withoutSmallFaces.byNumberOfFaces(6, 3).size should equal(46)
  }
  "byNumberOfFaces" should "find 112 diagrams in D[7,0]" in {
    val graphs = withoutSmallFaces.byNumberOfFaces(7, 0)
    for(g <- graphs) {
      g.numberOfInternalFaces should equal (0)
    }
    graphs.size should equal(112)
  }
  "byNumberOfFaces" should "find 155 diagrams in D[7,1]" in {
    withoutSmallFaces.byNumberOfFaces(7, 1).size should equal(155)
  }
}