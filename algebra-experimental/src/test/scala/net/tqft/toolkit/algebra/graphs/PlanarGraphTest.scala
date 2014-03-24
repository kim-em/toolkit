package net.tqft.toolkit.algebra.graphs

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

@RunWith(classOf[JUnitRunner])
class PlanarGraphTest extends FlatSpec with Matchers with IsomorphismMatchers {
  "PlanarGraph" should "do some computations" in {

    val spider = implicitly[Spider[PlanarGraph]]
    
    PlanarGraph.loop should be_isomorphic_to(spider.stitch(PlanarGraph.strand))
    spider.normSquared(PlanarGraph.strand) should be_isomorphic_to(PlanarGraph.loop)

    val twoStrands = spider.tensor(PlanarGraph.strand, PlanarGraph.strand)
    twoStrands shouldNot be_isomorphic_to(spider.rotate(twoStrands, 1))
  }
}

trait IsomorphismMatchers {
  class PlanarGraphIsomorphismMatcher(right: PlanarGraph) extends Matcher[PlanarGraph] {
    val spider = implicitly[Spider[PlanarGraph]]

    def apply(left: PlanarGraph) = {
      MatchResult(
        spider.canonicalForm(left) == spider.canonicalForm(right),
        s"""Graph $left is not isomorphic to $right""",
        s"""Graph $left is isomorphic to $right""""
      )
    }
  }

  def be_isomorphic_to(right: PlanarGraph) = new PlanarGraphIsomorphismMatcher(right)
}
