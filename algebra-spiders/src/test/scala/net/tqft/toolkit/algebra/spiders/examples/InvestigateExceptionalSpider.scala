package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.mathematica.Determinant
import net.tqft.toolkit.amazon.S3
import net.tqft.toolkit.algebra.spiders.VertexType
import net.tqft.toolkit.algebra.spiders.PlanarGraph

object InvestigateExceptionalSpider extends App {

  Determinant.cache = S3("determinants")

  val ring: Ring[MultivariablePolynomial[Fraction[BigInt], String]] = implicitly

  val exceptionalSpider = QuotientSpider(
    Seq((VertexType(3, 0, 1), ring.one), (VertexType(4, 0, 2), ring.one)),
    QuantumExceptionalSeries.reductions)

  val invertible: Seq[MultivariablePolynomial[Fraction[BigInt], String]] = Seq()

  val independentDiagrams = Seq[(Int, Map[VertexType, Int])](
    (0, Map()),
    (2, Map()),
    (3, Map(VertexType(3, 0, 1) -> 1)),
    (4, Map()),
    (4, Map(VertexType(3, 0, 1) -> 2)),
    (4, Map(VertexType(4, 0, 2) -> 1))).flatMap(p => exceptionalSpider.reducedDiagrams(p._1, p._2)).init

  val initialData = {
    val base = SpiderData(
      exceptionalSpider,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      dimensionBounds = Seq(1, 0, 1, 1, 5, 16),
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Set.empty)

    val withInverses = invertible.foldLeft(base)(_.invertPolynomial(_).get._1)
    val withIndependentDiagrams = independentDiagrams.foldLeft(Seq(withInverses))({
      case (s, d) =>
        s.flatMap(_.considerDiagram(d, independent_? = true))
    })

    withIndependentDiagrams
  }

  initialData.flatMap(s => s.considerDiagrams(4, Map(VertexType(4, 0, 2) -> 1)))
}