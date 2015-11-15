package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.graphs.Dreadnaut

@RunWith(classOf[JUnitRunner])
class PlanarGraphTest extends FlatSpec with Matchers with IsomorphismMatchers {
  val spider = implicitly[DiagramSpider[PlanarGraph]]

  import PlanarGraph._

  "empty graphs with different face labels" should "be the same" in {
    def g(i: Int) = PlanarGraph(i, IndexedSeq(IndexedSeq()), IndexedSeq.empty, 0)

    g(0) should be_isomorphic_to(g(1))
  }

  "a stitched strand" should "be a loop" in {
    loop should be_isomorphic_to(spider.stitch(strand))
  }

  "the empty diagram" should "be the tensor identity" in {
    spider.tensor(PlanarGraph.empty, strand) should be_isomorphic_to(strand)
  }

  "a rotated strand" should "be a strand" in {
    spider.rotate(strand, 1) should be_isomorphic_to(strand)
  }

  "a rotated trivalent vertex" should "not be a trivalent vertex" in {
    spider.rotate(star(3), 1) shouldNot be_isomorphic_to(star(3))
  }

  "a rotated trivalent vertex" should "be rotationally equivalent to a trivalent vertex" in {
    spider.rotate(star(3), 1) should be_rotationally_equivalent_to(star(3))
  }

  "a rotated crossing" should "not be rotationally equivalent to a crossing" in {
    val s1 = star(4, 0, 2)
    val s2 = spider.rotate(s1, 1)

    println(s1)
    println(s1.relabelEdgesAndFaces)
    println(s1.relabelEdgesAndFaces.nautyGraph)
    println(s1.relabelEdgesAndFaces.nautyGraph.adjacencies.zipWithIndex)
    println(Dreadnaut.canonicalLabelling(s1.relabelEdgesAndFaces.nautyGraph))
    println(spider.canonicalFormWithDefect(s1))
    println(spider.canonicalFormWithDefect(s1)._1.nautyGraph)
    println()
    println(s2)
    println(s2.relabelEdgesAndFaces)
    println(s2.relabelEdgesAndFaces.nautyGraph)
    println(s2.relabelEdgesAndFaces.nautyGraph.adjacencies.zipWithIndex)
    println(Dreadnaut.canonicalLabelling(s2.relabelEdgesAndFaces.nautyGraph))
    println(spider.canonicalFormWithDefect(s2))
    println(spider.canonicalFormWithDefect(s2)._1.nautyGraph)

    spider.rotate(star(4, 0, 2), 1) shouldNot be_rotationally_equivalent_to(star(4, 0, 2))
  }

  "a strand times a strand" should "be a strand" in {
    spider.multiply(strand, strand, 1) should be_isomorphic_to(strand)
  }

  "the norm of a strand" should "be a loop" in {
    spider.normSquared(strand) should be_isomorphic_to(loop)
  }

  "the two 2-strand Temperley-Lieb diagrams" should "not be isomorphic" in {
    val twoStrands = spider.tensor(strand, strand)
    twoStrands shouldNot be_isomorphic_to(spider.rotate(twoStrands, 1))
  }

  "a 2pi srotated trivalent vertex" should "be a trivalent vertex" in {
    star(3) should be_isomorphic_to(spider.rotate(star(3), 3))
  }

  "multiplying by a strand" should "do nothing" in {
    spider.multiply(strand, star(3), 1) should be_isomorphic_to(star(3))
  }

  "a polygon" should "be rotationally invariant" in {
    for (k <- 3 to 7) {
      polygon(k) should be_isomorphic_to(spider.rotate(polygon(k), 1))
    }
  }

  "a polygon" should "be built out of trivalent vertices" in {
    def p(m: Int) = spider.stitch(spider.rotate(Seq.fill(m)(star(3)).fold(strand)(spider.multiply(_, _, 1)), 1))
    for (k <- 0 to 7) {
      p(k) should be_isomorphic_to(polygon(k))
    }
  }

  "two identical diagrams" should "be isomorphic" in {
    val d1 = PlanarGraph(9, IndexedSeq(List((6, 9), (8, 12), (4, 10), (5, 9), (7, 13), (3, 11)), List((3, 9), (7, 11), (5, 13)), List((4, 9), (8, 10), (6, 12))), IndexedSeq((1, 0), (1, 0)), 0)
    val d2 = PlanarGraph(9, IndexedSeq(List((5, 9), (7, 13), (3, 11), (6, 9), (8, 12), (4, 10)), List((3, 9), (7, 11), (5, 13)), List((4, 9), (8, 10), (6, 12))), IndexedSeq((1, 0), (1, 0)), 0)
    d1 should be_isomorphic_to(d2)
  }

  "a monogon" should "have a geodesic from the inner face to the outer face" in {
    polygon(1).faceBoundary(5) should equal(Seq(Seq((1, 3))))
    polygon(1).faceNeighbours(5) should equal(Seq(Seq((3, 4))))
    polygon(1).geodesics(5).size should equal(1)
  }

  "stitch" should "not blow up" in {
    spider.stitch(PlanarGraph(9, IndexedSeq(List((4, 9), (3, 8), (11, 6), (11, 17), (8, 6), (2, 6)), List((2, 9), (3, 6), (4, 8)), List((8, 6), (5, 6), (5, 7))), IndexedSeq((1, 0), (1, 0)), 0)) should equal(PlanarGraph(9, IndexedSeq(List((4, 9), (3, 8), (11, 9), (11, 17)), List((2, 9), (3, 9), (4, 8)), List((2, 9), (5, 9), (5, 7))), IndexedSeq((1, 0), (1, 0)), 0))
  }

  "excisions" should "not blow up" in {
    val q = PlanarGraph(7, IndexedSeq(List((18, 7), (17, 10)), List((2, 8), (3, 7), (4, 9)), List((3, 9), (16, 7), (15, 7)), List((2, 7), (4, 8), (15, 9)), List((18, 10), (16, 7), (17, 7))), IndexedSeq((1, 0), (1, 0), (1, 0), (1, 0)), 0)
    q.Subgraphs(polygon(2)).excisions.size should equal(2)
  }
}

