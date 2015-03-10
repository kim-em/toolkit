package net.tqft.toolkit.algebra.spiders

import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

trait IsomorphismMatchers {
  class PlanarGraphIsomorphismMatcher(right: PlanarGraph) extends Matcher[PlanarGraph] {
    val spider = implicitly[DiagramSpider[PlanarGraph]]

    def apply(left: PlanarGraph) = {
      val leftCanonical = spider.canonicalFormWithDefect(left)
      val rightCanonical = spider.canonicalFormWithDefect(right)
      MatchResult(
        leftCanonical == rightCanonical,
        s"""\n$left\n ---> $leftCanonical\nis not isomorphic to\n$right\n ---> $rightCanonical""",
        s"""Graph $left is isomorphic to $right"""")
    }
  }
  class PlanarGraphRotationalEquivalenceMatcher(right: PlanarGraph) extends Matcher[PlanarGraph] {
    val spider = implicitly[DiagramSpider[PlanarGraph]]

    def apply(left: PlanarGraph) = {
      val leftCanonical = spider.canonicalFormWithDefect(left)
      val rightCanonical = spider.canonicalFormWithDefect(right)
      MatchResult(
        leftCanonical._1 == rightCanonical._1,
        s"""\n$left\n ---> $leftCanonical\nis not rotationally equivalent to\n$right\n ---> $rightCanonical""",
        s"""Graph $left is rotationally equivalent to $right"""")
    }
  }

  def be_isomorphic_to(right: PlanarGraph) = new PlanarGraphIsomorphismMatcher(right)
  def be_rotationally_equivalent_to(right: PlanarGraph) = new PlanarGraphRotationalEquivalenceMatcher(right)
}
