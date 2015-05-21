package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.mathematica.Determinant
import net.tqft.toolkit.amazon.S3
import net.tqft.toolkit.algebra.spiders.VertexType

object InvestigateTetravalentSpiders extends App {

  Determinant.cache = S3("determinants")

  val ring: Ring[MultivariablePolynomial[Fraction[BigInt], String]] = implicitly

  val lowestWeightTetravalentSpider = QuotientSpider.withLowestWeightGenerators(Seq((VertexType(4, 0, 1), ring.one)))

  val p1 = MultivariablePolynomial(Map(Map("p1" -> 1) -> Fraction[BigInt](1, 1)))
  val p2 = MultivariablePolynomial(Map(Map("p2" -> 1) -> Fraction[BigInt](1, 1)))

  val invertible: Seq[MultivariablePolynomial[Fraction[BigInt], String]] = Seq(p1,
    p2,
    ring.add(p1, 1), ring.add(p1, -1),
    ring.add(p2, -2) //,
    )

  val initialData = invertible.foldLeft(SpiderData(
    lowestWeightTetravalentSpider,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    dimensionBounds = Seq(1, 0, 1, 0, 3, 0, 14),
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Set.empty))(_.invertPolynomial(_).get._1)

  val steps = Seq((0, 0), (2, 0), (0, 1), (0, 2), (2, 1), (2, 2), (2, 3), (4, 0), (4, 1), (4, 2), (4, 3) /**/ , (6, 0), (6, 1) /**/ , (6, 2) /**/ )

  // TODO start computing relations, also

  val results = initialData.considerDiagrams(steps)

  //  for (r <- results) {
  //    println(r.innerProducts(r.consideredDiagrams(6)).toMathematicaInputString)
  //  }

  println(results.size)
}