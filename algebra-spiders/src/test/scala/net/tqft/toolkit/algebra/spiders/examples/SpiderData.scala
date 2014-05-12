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
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebras

case class SpiderData(
  spider: MultivariableRationalFunctionSpider[Fraction[BigInt]],
  polyhedra: Seq[String],
  groebnerBasis: Seq[MultivariablePolynomial[Fraction[BigInt], String]],
  nonzero: Seq[MultivariablePolynomial[Fraction[BigInt], String]],
  relations: Seq[Seq[Map[PlanarGraph, MultivariablePolynomial[Fraction[BigInt], String]]]],
  dimensionBounds: Seq[Int],
  consideredDiagramsVertexBound: Seq[Int],
  consideredDiagrams: Seq[Seq[PlanarGraph]],
  independentDiagrams: Seq[Seq[PlanarGraph]]) {

  lazy val quotientRing = Field.fieldOfFractions(
    MultivariablePolynomialAlgebras.quotient(groebnerBasis))

  def considerDiagram(p: PlanarGraph): Seq[SpiderData] = {
    val boundary = p.numberOfBoundaryPoints
    val newConsideredDiagrams = {
      val padded = consideredDiagrams.padTo(boundary + 1, Seq.empty)
      padded.updated(boundary, (padded(boundary) :+ p).distinct)
    }
    val paddedIndependentDiagrams = independentDiagrams.padTo(boundary + 1, Seq.empty)
    val candidateIndependentDiagrams = paddedIndependentDiagrams(boundary) :+ p
    val matrix = spider.innerProductMatrix(candidateIndependentDiagrams, candidateIndependentDiagrams)
    val determinant = Matrix(candidateIndependentDiagrams.size, matrix).determinant(quotientRing).ensuring(_.denominator == quotientRing.one.denominator).numerator
    
    val addIndependentDiagram: Option[SpiderData] = {
      if (determinant != quotientRing.zero && candidateIndependentDiagrams.size <= dimensionBounds(boundary)) {
        // TODO update nonzero
        val newNonzero = {
          import
          val factors = determinant.factor
          ???
        }
        val newIndependentDiagrams = paddedIndependentDiagrams.updated(boundary, candidateIndependentDiagrams)
        Some(copy(nonzero = newNonzero,
          consideredDiagrams = newConsideredDiagrams,
          independentDiagrams = newIndependentDiagrams))
      } else {
        None
      }
    }
    val addDeterminantToIdeal: Option[SpiderData] = {
      val newGroebnerBasis = {
        import mathematica.GroebnerBasis._
        (groebnerBasis :+ determinant).computeGroebnerBasis
      }
      Some(copy(
        consideredDiagrams = newConsideredDiagrams,
        groebnerBasis = newGroebnerBasis))
    }

    addIndependentDiagram.toSeq ++ addDeterminantToIdeal
  }

  def considerDiagrams(boundary: Int, vertices: Int): Seq[SpiderData] = {
    val newConsideredDiagramsVertexBound = consideredDiagramsVertexBound.padTo(boundary + 1, 0).updated(boundary, vertices)
    val diagramsToConsider = spider.reducedDiagrams(boundary, vertices)

    diagramsToConsider.foldLeft(Seq(this))({ (s: Seq[SpiderData], p: PlanarGraph) => s.flatMap(d => d.considerDiagram(p)) })
      .map(_.copy(consideredDiagramsVertexBound = newConsideredDiagramsVertexBound))
  }
}

object InvestigateTetravalentSpiders extends App {
  val freeTetravalentSpider = new FreeSpider {
    override def generators = Seq((VertexType(4, 1), ring.one))
  }

  val initialData = SpiderData(
    freeTetravalentSpider,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    dimensionBounds = Seq(1, 0, 1, 0, 3),
    Seq.empty,
    Seq.empty,
    Seq.empty)

  println(Seq(initialData).flatMap(_.considerDiagrams(0, 0)).flatMap(_.considerDiagrams(0, 2)))
}