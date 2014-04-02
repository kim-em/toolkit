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
}

