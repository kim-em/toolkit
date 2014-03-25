package net.tqft.toolkit.algebra.graphs

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

@RunWith(classOf[JUnitRunner])
class PlanarGraphTest extends FlatSpec with Matchers with IsomorphismMatchers {
  val spider = implicitly[Spider[PlanarGraph]]

  import PlanarGraph._

//  "a stitched strand" should "be a loop" in {
//    loop should be_isomorphic_to(spider.stitch(strand))
//  }
//
//  "a rotated strand" should "be a strand" in {
//    spider.rotate(strand, 1) should be_isomorphic_to(strand)
//  }
//
//  "a rotated trivalent vertex" should "not be a trivalent vertex" in {
//    spider.rotate(star(3), 1) shouldNot be_isomorphic_to(star(3))
//  }
//  
  "a rotated trivalent vertex" should "be rotationally equivalent to a trivalent vertex" in {
    spider.rotate(star(3), 1) should be_rotationally_equivalent_to(star(3))
  }
//  
//  "a strand times a strand" should "be a strand" in {
//    spider.multiply(strand, strand, 1) should be_isomorphic_to(strand)
//  }
//
//  "the norm of a strand" should "be a loop" in {
//    spider.normSquared(strand) should be_isomorphic_to(loop)
//  }
//
//  "the two 2-strand Temperley-Lieb diagrams" should "not be isomorphic" in {
//    val twoStrands = spider.tensor(strand, strand)
//    twoStrands shouldNot be_isomorphic_to(spider.rotate(twoStrands, 1))
//  }
//
//  "a 2pi srotated trivalent vertex" should "be a trivalent vertex" in {
//    star(3) should be_isomorphic_to(spider.rotate(star(3), 3))
//  }
//
//  "multiplying by a strand" should "do nothing" in {
//    spider.multiply(strand, star(3), 1) should be_isomorphic_to(star(3))
//  }
//
//  "a pentagon" should "be rotationally invariant" in {
//    polygon(5) should be_isomorphic_to(spider.rotate(polygon(5), 1))
//  }
//
//  "a polygon" should "be built out of trivalent vertices" in {
//    def p(m: Int) = spider.stitch(spider.rotate(Seq.fill(m)(star(3)).fold(strand)(spider.multiply(_, _, 1)), 1))
//
//    for (k <- 0 to 7) {
//      p(k) should be_isomorphic_to(polygon(k))
//    }
//  }
}

trait IsomorphismMatchers {
  class PlanarGraphIsomorphismMatcher(right: PlanarGraph) extends Matcher[PlanarGraph] {
    val spider = implicitly[Spider[PlanarGraph]]

    def apply(left: PlanarGraph) = {
      val leftCanonical = spider.canonicalForm(left)
      val rightCanonical = spider.canonicalForm(right)
      MatchResult(
        leftCanonical == rightCanonical,
        s"""\n$left\n ---> $leftCanonical\nis not isomorphic to\n$right\n ---> $rightCanonical""",
        s"""Graph $left is isomorphic to $right"""")
    }
  }
  class PlanarGraphRotationalEquivalenceMatcher(right: PlanarGraph) extends Matcher[PlanarGraph] {
    val spider = implicitly[Spider[PlanarGraph]]

    def apply(left: PlanarGraph) = {
      val leftCanonical = spider.canonicalForm(left)
      val rightCanonical = spider.canonicalForm(right)
      MatchResult(
        leftCanonical._1 == rightCanonical._1,
        s"""\n$left\n ---> $leftCanonical\nis not rotationally equivalent to\n$right\n ---> $rightCanonical""",
        s"""Graph $left is rotationally equivalent to $right"""")
    }
  }

  def be_isomorphic_to(right: PlanarGraph) = new PlanarGraphIsomorphismMatcher(right)
  def be_rotationally_equivalent_to(right: PlanarGraph) = new PlanarGraphRotationalEquivalenceMatcher(right)
}
