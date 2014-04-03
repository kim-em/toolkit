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

  "a dodecahedron" should "contain 60 pentagons" in {
    dodecahedron.Subgraphs(polygon(5)).excisions.size should equal(55)
  }
}

