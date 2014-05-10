package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.spiders.MultivariableRationalFunctionSpider
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.spiders.PlanarGraph
import net.tqft.toolkit.collections.KSubsets
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.mathematica._
import net.tqft.toolkit.algebra.spiders.VertexType
import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebra

case class SpiderData(
  spider: MultivariableRationalFunctionSpider[Fraction[BigInt]],
  polyhedra: Seq[String],
  groebnerBasis: Seq[MultivariablePolynomial[Fraction[BigInt], String]],
  relations: Seq[Seq[Map[PlanarGraph, MultivariablePolynomial[Fraction[BigInt], String]]]],
  dimensionBounds: Seq[Int],
  consideredDiagramsVertexBound: Seq[Int],
  consideredDiagrams: Seq[Seq[PlanarGraph]],
  preferredSpanningSet: Seq[Option[Seq[PlanarGraph]]],
  preferredBasis: Seq[Option[Seq[PlanarGraph]]]) {
  
  def considerDiagrams(boundary: Int, vertices: Int): SpiderData = {
    val newConsideredDiagramsVertexBound = consideredDiagramsVertexBound.padTo(boundary + 1, 0).updated(boundary, vertices)
    val newConsideredDiagrams = {
      val padded = consideredDiagrams.padTo(boundary + 1, Seq.empty)
      padded.updated(boundary, (padded(boundary) ++ spider.reducedDiagrams(boundary, vertices)).distinct)
    }
    val diagramsToConsider = newConsideredDiagrams(boundary)

    if (diagramsToConsider.size <= dimensionBounds(boundary)) {
      // nothing to see here
      copy(consideredDiagramsVertexBound = newConsideredDiagramsVertexBound, consideredDiagrams = newConsideredDiagrams)
    } else {
      // time to compute some determinants!
      import KSubsets._
      val subsets = diagramsToConsider.kSubsets(dimensionBounds(boundary) + 1)
      val matrix = spider.innerProductMatrix(diagramsToConsider, diagramsToConsider)
      val matrices = (for (subset <- KSubsets(diagramsToConsider.size, dimensionBounds(boundary) + 1)) yield {
        val entries = subset.map(matrix).map(r => subset.map(r))
        Matrix(dimensionBounds(boundary) + 1, entries)
      })
      val determinants = matrices.map(_.determinant.ensuring(_.denominator == implicitly[MultivariablePolynomialAlgebra[BigInt, String]].one).numerator)

      val newGroebnerBasis = {
        import mathematica.GroebnerBasis._
        (groebnerBasis ++ determinants).computeGroebnerBasis
      }
      
      // TODO we should also look for new relations
      
      copy(
          consideredDiagramsVertexBound = newConsideredDiagramsVertexBound,
          consideredDiagrams = newConsideredDiagrams,
          groebnerBasis = newGroebnerBasis)
    }
  }
}

object InvestigateTetravalentSpiders extends App {
  val freeTetravalentSpider = new FreeSpider {
    override def generators = Seq((VertexType(4, 1), ring.one))
  }

  val initialData = SpiderData(freeTetravalentSpider, Seq.empty, Seq.empty, Seq.empty, dimensionBounds = Seq(1, 0, 1, 0, 3), Seq.empty, Seq.empty, Seq.empty, Seq.empty)

  println(initialData.considerDiagrams(0, 0).considerDiagrams(0, 2))
}