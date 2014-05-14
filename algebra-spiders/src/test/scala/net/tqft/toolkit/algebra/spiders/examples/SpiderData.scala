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
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebraOverField
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

  override def toString = {
    s"""SpiderData(
  groebnerBasis = $groebnerBasis,
  nonzero = $nonzero,
  consideredDiagramsVertexBound = $consideredDiagramsVertexBound,
  consideredDiagrams = $consideredDiagrams,
  independentDiagrams = $independentDiagrams
)"""
  }

  def polynomials = MultivariablePolynomialAlgebras.quotient(groebnerBasis)
  lazy val rationalFunctions = Field.fieldOfFractions(polynomials)

  def considerDiagram(p: PlanarGraph): Seq[SpiderData] = {
    val boundary = p.numberOfBoundaryPoints
    val newConsideredDiagrams = {
      val padded = consideredDiagrams.padTo(boundary + 1, Seq.empty)
      padded.updated(boundary, (padded(boundary) :+ p).distinct)
    }
    val paddedIndependentDiagrams = independentDiagrams.padTo(boundary + 1, Seq.empty)
    val candidateIndependentDiagrams = paddedIndependentDiagrams(boundary) :+ p
    val matrix = spider.innerProductMatrix(candidateIndependentDiagrams, candidateIndependentDiagrams)
    val determinant = Matrix(candidateIndependentDiagrams.size, matrix).determinant(rationalFunctions).ensuring(_.denominator == polynomials.one).numerator

    val addIndependentDiagram: Option[SpiderData] = {
      if (determinant != polynomials.zero && candidateIndependentDiagrams.size <= dimensionBounds(boundary)) {
        val newNonzero = {
          import mathematica.Factor._
          val factors = determinant.factor.filterNot(_._1 == polynomials.one)
          (nonzero ++ factors.keys).distinct
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

      // has everything collapsed?
      if (newGroebnerBasis.contains(polynomials.one)) {
        None
      } else {
        // does this kill anything in nonzero? 
        val newNonzero = nonzero.map(MultivariablePolynomialAlgebras.quotient(newGroebnerBasis).normalForm)
        if (newNonzero.exists(_ == polynomials.zero)) {
          None
        } else {
          Some(copy(
            consideredDiagrams = newConsideredDiagrams,
            groebnerBasis = newGroebnerBasis,
            nonzero = newNonzero))
        }
      }
    }

    addIndependentDiagram.toSeq ++ addDeterminantToIdeal
  }

  def considerDiagrams(boundary: Int, vertices: Int): Seq[SpiderData] = {
    val newConsideredDiagramsVertexBound = consideredDiagramsVertexBound.padTo(boundary + 1, 0).updated(boundary, vertices)
    val diagramsToConsider = spider.reducedDiagrams(boundary, vertices)
    for(d <- diagramsToConsider) println("   " + d)

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

    val steps = Seq((0,0), (2,0), (0,2), (2,1))
    
    
    // TODO evaluate disjoint unions correctly
    // TODO graphs with capped vertices are not being generated, even though we don't have a relation reducing those
    // TODO make generators uncappable automatically?
    // TODO declare d nonzero?
    
    steps.foldLeft(Seq(initialData))({ (data: Seq[SpiderData], step: (Int, Int)) =>
      println(s"Considering diagrams with ${step._1} boundary points and ${step._2} vertices...")
      val result = data.flatMap(_.considerDiagrams(step._1, step._2))
      for(s <- result) println(s)
      result
    })
    
}