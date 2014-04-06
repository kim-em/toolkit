package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.matrices2.Matrix

@RunWith(classOf[JUnitRunner])
class InnerProductsTest extends FlatSpec with Matchers with IsomorphismMatchers {

  val `D(4,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(4, 0)

  "inner products of D(4,0)" should "be correct" in {
    val spider = TrivalentSpider
    val `M(4,0)` = spider.innerProductMatrix(`D(4,0)`.toIndexedSeq)

    val d = "d"
    val b = "b"
    val t = "t"
    val result: List[List[MultivariablePolynomial[Fraction[Int], String]]] = List(List(MultivariablePolynomial(Map(Map(d -> 2) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map()), MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> Fraction(1, 1)))), List(MultivariablePolynomial(Map(Map(d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(d -> 2) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map())), List(MultivariablePolynomial(Map()), MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(b -> 2, d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(t -> 1, b -> 1, d -> 1) -> Fraction(1, 1)))), List(MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map()), MultivariablePolynomial(Map(Map(t -> 1, b -> 1, d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(b -> 2, d -> 1) -> Fraction(1, 1)))))

    `M(4,0)` should equal(result)
  }

  "twisted inner products of D(4,0)" should "be correct" in {
    val diagramSpider = implicitly[DiagramSpider[PlanarGraph]]
    val spider = TwistedTrivalentSpider
    import spider.ring
    val x = diagramSpider.innerProduct(
      PlanarGraph.H,
      diagramSpider.tensor(PlanarGraph.strand, PlanarGraph.strand))
    println(x)
    val subgraphs = x.Subgraphs(PlanarGraph.polygon(2))
    for(s <- subgraphs.excisions) println(s)
    val xc = spider.canonicalForm(Map(x -> ring.one))
    println(xc)
  }
}

