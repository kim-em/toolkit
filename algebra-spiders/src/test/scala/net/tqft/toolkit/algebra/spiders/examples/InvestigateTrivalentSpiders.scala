package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.mathematica.Determinant
import net.tqft.toolkit.amazon.S3
import net.tqft.toolkit.algebra.spiders.VertexType

object InvestigateTrivalentSpiders extends App {

  Determinant.cache = S3("determinants")

  val lowestWeightTrivalentSpider = (new LowestWeightSpider {
    override def generators = Seq((VertexType(3, 1), ring.one))
  }).asQuotientSpider

  val ring: Ring[MultivariablePolynomial[Fraction[BigInt], String]] = implicitly

  val p1 = MultivariablePolynomial(Map(Map("p1" -> 1) -> Fraction[BigInt](1, 1)))
  val p2 = MultivariablePolynomial(Map(Map("p2" -> 1) -> Fraction[BigInt](1, 1)))

  val invertible: Seq[MultivariablePolynomial[Fraction[BigInt], String]] = Seq(p1, p2)

  val initialData = invertible.foldLeft(SpiderData(
    lowestWeightTrivalentSpider,
    Seq.empty,
    Seq.empty,
    dimensionBounds = Seq(1, 0, 1, 1, 4, 10),
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty))(_.invertPolynomial(_).get._1)

  val steps = Seq((0, 0), (2, 0), (0, 2), (2, 2), (3, 1), (3, 3), (4, 0), (4, 2), (4, 4) /**/ , (5, 1), (5, 3) /**/ , (5, 5) /**/ )

  // TODO start computing relations, also

  val simplifyBigons = initialData.considerDiagrams(Seq((0, 0), (2, 0), (0, 2), (2, 2)))
  for (s <- simplifyBigons) { require(s.spider.extraReductions.size == 1) }
  val simplifyTriangles = simplifyBigons.flatMap(_.considerDiagrams(Seq((3, 1), (3, 3))))
  for (s <- simplifyTriangles) {
    println(s)
    require(s.spider.extraReductions.size == 2)
  }
    val simplifySquares = simplifyTriangles.flatMap(_.considerDiagrams(Seq((4, 0), (4, 2), (4, 4))))
  for (s <- simplifySquares) {
    println(s)
    // TODO actually, just insist the square is reducible
    require(s.spider.extraReductions.size == 3)
  }

}