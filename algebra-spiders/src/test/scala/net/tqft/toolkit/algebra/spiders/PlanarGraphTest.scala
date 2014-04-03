package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PlanarGraphTest extends FlatSpec with Matchers with IsomorphismMatchers {
  val spider = implicitly[DiagramSpider[PlanarGraph]]

  import PlanarGraph._

  "empty graphs with different face labels" should "be the same" in {
    def g(i: Int) = PlanarGraph(i, IndexedSeq(IndexedSeq()), 0)
    
    g(0) should be_isomorphic_to(g(1))
  }
  
  "a stitched strand" should "be a loop" in {
    loop should be_isomorphic_to(spider.stitch(strand))
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
    for(k <- 3 to 7) {
    polygon(k) should be_isomorphic_to(spider.rotate(polygon(k), 1))
    }
  }

  "a polygon" should "be built out of trivalent vertices" in {
    def p(m: Int) = spider.stitch(spider.rotate(Seq.fill(m)(star(3)).fold(strand)(spider.multiply(_, _, 1)), 1))

    for (k <- 0 to 7) {
      p(k) should be_isomorphic_to(polygon(k))
    }
  }
  
  "a monogon" should "have a geodesic from the inner face to the outer face" in {
    polygon(1).faceBoundary(5) should equal(Seq((1,3)))
    polygon(1).faceNeighbours(5) should equal(Seq((3,4)))
    polygon(1).geodesics(5).size should equal(1)
  }
}

