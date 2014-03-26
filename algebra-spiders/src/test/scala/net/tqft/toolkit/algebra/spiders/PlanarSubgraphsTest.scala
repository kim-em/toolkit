package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PlanarSubgraphTest extends FlatSpec with Matchers with IsomorphismMatchers {
  val spider = implicitly[Spider[PlanarGraph]]

  import PlanarGraph._

  "a k-gon" should "contain 3k trivalent vertices" in {
    for (k <- 3 to 7) {
      polygon(k).Subgraphs(star(3)).excisions.size should equal(3 * k)
    }
  }
  "a k-gon" should "contain k Is" in {
    for (k <- 3 to 7) {
      polygon(k).Subgraphs(I).excisions.size should equal(k)
    }
  }
  "a k-gon" should "contain k Hs" in {
    for (k <- 3 to 7) {
      polygon(k).Subgraphs(H).excisions.size should equal(k)
    }
  }
}

