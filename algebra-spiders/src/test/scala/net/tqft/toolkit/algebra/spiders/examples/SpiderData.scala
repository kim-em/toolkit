package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.mathematica._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.collections.KSubsets
import net.tqft.toolkit.algebra.matrices.Matrix

case class SpiderData(
  spider: QuotientSpider,
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

  val complexity: Ordering[PlanarGraph] = {
    Ordering.by(_.numberOfInternalVertices)
  }

  lazy val polynomials = MultivariablePolynomialAlgebras.quotient(groebnerBasis)
  lazy val rationalFunctions = Field.fieldOfFractions(polynomials)

  def considerDiagram(p: PlanarGraph): Seq[SpiderData] = {
    val boundary = p.numberOfBoundaryPoints
    val newConsideredDiagrams = {
      val padded = consideredDiagrams.padTo(boundary + 1, Seq.empty)
      padded.updated(boundary, (padded(boundary) :+ p).distinct)
    }
    val paddedIndependentDiagrams = independentDiagrams.padTo(boundary + 1, Seq.empty)
    val oldIndependentDiagrams = paddedIndependentDiagrams(boundary)
    val candidateIndependentDiagrams = oldIndependentDiagrams :+ p

    // FIXME the spider should know the ring, so the inner product automatically happens mod the ideal.
    lazy val rectangularMatrix = spider.innerProductMatrix(oldIndependentDiagrams, candidateIndependentDiagrams).map(row => row.map(entry => Fraction(polynomials.normalForm(entry.numerator), polynomials.normalForm(entry.denominator))))
    lazy val lastRow = spider.innerProductMatrix(Seq(p), candidateIndependentDiagrams).map(row => row.map(entry => Fraction(polynomials.normalForm(entry.numerator), polynomials.normalForm(entry.denominator))))
    def matrix = rectangularMatrix ++ lastRow
    lazy val determinant = Matrix(candidateIndependentDiagrams.size, matrix).determinant(rationalFunctions).ensuring(_.denominator == polynomials.one).numerator

    val addIndependentDiagram: Option[SpiderData] = {
      if (candidateIndependentDiagrams.size <= dimensionBounds(boundary)) {
        val newIndependentDiagrams = paddedIndependentDiagrams.updated(boundary, candidateIndependentDiagrams)
        declarePolynomialNonzero(determinant).map(_.copy(
          consideredDiagrams = newConsideredDiagrams,
          independentDiagrams = newIndependentDiagrams))
      } else {
        None
      }
    }
    val addDependentDiagram: Seq[SpiderData] = {
      val relation = Matrix(oldIndependentDiagrams.size + 1, rectangularMatrix).nullSpace(rationalFunctions).ensuring(_.size == 1).head
      require(relation.last == rationalFunctions.one)

      // is it a reducing relation?
      val nonzeroPositions = relation.dropRight(1).zipWithIndex.collect({ case (x, i) if x != rationalFunctions.zero => i })
      val reducing = nonzeroPositions.forall({ i => complexity.lt(oldIndependentDiagrams(i), p) })

      if (reducing) {
        // are there denominators? we better ensure they are invertible
        val denominatorLCM = polynomials.lcm(relation.map(_.denominator): _*)

        val whenDenominatorsVanish = declarePolynomialZero(denominatorLCM).toSeq.flatMap(_.considerDiagram(p))
        val whenDenominatorsNonzero = declarePolynomialNonzero(denominatorLCM).map(
          _.copy(spider = spider.addReduction(
            Reduction(
              p,
              oldIndependentDiagrams.zip(relation.dropRight(1).map(rationalFunctions.negate)).toMap))))

        whenDenominatorsVanish ++ whenDenominatorsNonzero
      } else {
        // hmm... just ask that the determinant vanishes
        // TODO record non-reducing relations!
        declarePolynomialZero(determinant).toSeq.map(_.copy(consideredDiagrams = newConsideredDiagrams))
      }

    }

    addIndependentDiagram.toSeq ++ addDependentDiagram
  }

  def declarePolynomialNonzero(p: MultivariablePolynomial[Fraction[BigInt], String]): Option[SpiderData] = {
    if (p != polynomials.zero) {
      val newNonzero = {
        import mathematica.Factor._
        val factors = p.factor.filterNot(_._1 == polynomials.one)
        (nonzero ++ factors.keys).distinct
      }
      Some(copy(nonzero = newNonzero))
    } else {
      None
    }

  }

  def declarePolynomialZero(r: MultivariablePolynomial[Fraction[BigInt], String]): Seq[SpiderData] = {
    val factors = {
      import mathematica.Factor._
      r.factor.keys.toSeq.ensuring(_.nonEmpty)
    }
    factors.flatMap(declareIrreduciblePolynomialZero)
  }

  def declareIrreduciblePolynomialZero(r: MultivariablePolynomial[Fraction[BigInt], String]): Option[SpiderData] = {
    val newGroebnerBasis = {
      import mathematica.GroebnerBasis._
      (groebnerBasis :+ r).computeGroebnerBasis
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
          groebnerBasis = newGroebnerBasis,
          nonzero = newNonzero))
      }
    }

  }

  def considerDiagrams(boundary: Int, vertices: Int): Seq[SpiderData] = {
    println(s"Considering diagrams with $boundary boundary points and $vertices vertices...")

    val newConsideredDiagramsVertexBound = consideredDiagramsVertexBound.padTo(boundary + 1, 0).updated(boundary, vertices)
    val diagramsToConsider = spider.reducedDiagrams(boundary, vertices)
    for (d <- diagramsToConsider) println("   " + d)

    diagramsToConsider.foldLeft(Seq(this))({ (s: Seq[SpiderData], p: PlanarGraph) => s.flatMap(d => d.considerDiagram(p)) })
      .map(_.copy(consideredDiagramsVertexBound = newConsideredDiagramsVertexBound))
  }

  def considerDiagrams(diagramSizes: Seq[(Int, Int)]): Seq[SpiderData] = {
    diagramSizes.foldLeft(Seq(this))({ (data: Seq[SpiderData], step: (Int, Int)) =>
      val result = data.flatMap(_.considerDiagrams(step._1, step._2))
      for (s <- result) println(s)
      result
    })
  }
}

object InvestigateTetravalentSpiders extends App {
  val lowestWeightTetravalentSpider = (new LowestWeightSpider {
    override def generators = Seq((VertexType(4, 1), ring.one))
  }).asQuotientSpider

  val initialData = SpiderData(
    lowestWeightTetravalentSpider,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    dimensionBounds = Seq(1, 0, 1, 0, 3),
    Seq.empty,
    Seq.empty,
    Seq.empty)

  val steps = Seq((0, 0), (2, 0), (0, 1), (0, 2), (2, 1), (2, 2), (4, 0), (4, 1))

  // TODO declare d nonzero?
  // TODO start computing relations, also
  // TODO have SpiderData compute lower bounds on ranks
  // TODO have consider diagram automatically call considerDiagrams for fewer vertices, if they haven't already been done.

  initialData.considerDiagrams(steps)

}