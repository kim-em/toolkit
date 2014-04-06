package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PlanarSubgraphTest extends FlatSpec with Matchers with IsomorphismMatchers {
  val spider = implicitly[Spider[PlanarGraph]]

  import PlanarGraph._

  "a k-gon" should "contain 3k trivalent vertices" in {
    for (k <- 1 to 7) {
      val q = polygon(k)
      q.Subgraphs(star(3)).excisions.size should equal(3 * k)
    }
  }
  "a monogon" should "contain no Is" in {
    val q = polygon(1)
    q.Subgraphs(I).excisions.size should equal(0)
  }
  "a k-gon with k >= 2" should "contain 2k Is" in {
    for (k <- 2 to 7) {
      val q = polygon(k)
      q.Subgraphs(I).excisions.size should equal(2 * k)
    }
  }
  "a k-gon with k >= 2" should "contain 2k Hs" in {
    for (k <- 2 to 7) {
      val q = polygon(k)
      q.Subgraphs(H).excisions.size should equal(2 * k)
    }
  }
  "a k-gon" should "map to itself k times" in {
    for (k <- 1 to 7) {
      val q = polygon(k)
      q.Subgraphs(q).excisions.size should equal(k)
    }
  }
  "some diagrams with monogons" should "contain monogons" in {
    val d1 = PlanarGraph(11,IndexedSeq(List((3,11), (6,10), (5,10), (4,8)), List((6,10), (7,10), (7,9)), List((3,10), (4,11), (5,8))),0)
    d1.Subgraphs(polygon(1)).excisions.size should equal(1)
    val d2 = PlanarGraph(8,IndexedSeq(List((3,8), (5,10), (4,9), (3,10)), List((4,10), (7,9), (5,9)), List((6,9), (6,11), (7,9))),0)
    d2.Subgraphs(polygon(1)).excisions.size should equal(1)
  }
  "a theta" should "contain ..." in {
    theta.Subgraphs(star(3)).excisions.size should equal(6)
    theta.Subgraphs(I).excisions.size should equal(6)
    theta.Subgraphs(polygon(2)).excisions.size should equal(4)
  } 
  "a tetrahedron" should "contain ..." in {
    tetrahedron.Subgraphs(star(3)).excisions.size should equal(12)
    tetrahedron.Subgraphs(I).excisions.size should equal(12)
    tetrahedron.Subgraphs(polygon(3)).excisions.size should equal(9)
  }

  "a dodecahedron" should "contain 55 pentagons" in {
    dodecahedron.Subgraphs(polygon(5)).excisions.size should equal(55)
  }
  
  "a theta inside another theta" should "only contain 6 bigons" in {
    val q = PlanarGraph(9,IndexedSeq(List(), List((4,13), (8,10), (6,9)), List((3,13), (7,11), (5,12)), List((5,13), (7,12), (3,11)), List((6,13), (8,9), (4,10))),0)
    q.Subgraphs(polygon(2)).excisions.size should equal(6)
  }
  
  "this graph" should "not blow up while finding subgraphs" in {
    val q = PlanarGraph(12,IndexedSeq(List(), List((8,13), (8,12), (11,13)), List((7,15), (11,13), (9,13)), List((4,16), (5,13), (6,17)), List((4,13), (23,16), (22,13)), List((7,13), (9,15), (22,13)), List((5,17), (23,13), (6,16))),0)
    q.Subgraphs(polygon(2)).excisions.size should equal(4)
  }
}

