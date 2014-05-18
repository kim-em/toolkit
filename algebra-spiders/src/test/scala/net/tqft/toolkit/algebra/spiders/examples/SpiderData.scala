package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.mathematica._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.collections.KSubsets
import net.tqft.toolkit.algebra.matrices.Matrix
import MathematicaForm._

case class SpiderData(
  spider: QuotientSpider,
  groebnerBasis: Seq[MultivariablePolynomial[Fraction[BigInt], String]],
  nonzero: Seq[MultivariablePolynomial[Fraction[BigInt], String]],
  relations: Seq[Seq[Map[PlanarGraph, MultivariablePolynomial[Fraction[BigInt], String]]]],
  dimensionBounds: Seq[Int],
  consideredDiagramsVertexBound: Seq[Int],
  consideredDiagrams: Seq[Seq[PlanarGraph]],
  independentDiagrams: Seq[Seq[PlanarGraph]]) {

  require(!nonzero.contains(polynomials.one))

  override def toString = {
    s"""SpiderData(
  groebnerBasis = ${groebnerBasis.toMathematicaInputString},
  nonzero = ${nonzero.toMathematicaInputString},
  dimensionBounds = $dimensionBounds,
  dimensionLowerBounds = $dimensionLowerBounds,
  allPolyhedra = $allPolyhedra,
  consideredDiagramsVertexBound = $consideredDiagramsVertexBound,
  consideredDiagrams = $consideredDiagrams,
  independentDiagrams = $independentDiagrams
)"""
  }

  def dimensionLowerBounds = independentDiagrams.map(_.size)
  def allPolyhedra =
    (groebnerBasis.flatMap(_.variables) ++
      nonzero.flatMap(_.variables) ++
      spider.reductions.flatMap(_.small.values.flatMap(r => r.numerator.variables ++ r.denominator.variables)) ++
      relations.flatMap(_.flatMap(_.values.flatMap(_.variables)))).distinct

  def reevaluateAllPolyhedra: Seq[SpiderData] = {
    val differences = (for (p <- allPolyhedra.iterator; g <- PolyhedronNamer.byName(p).iterator; r <- spider.allEvaluations(g)) yield {
      rationalFunctions.subtract(Fraction.whole(polynomials.monomial(p)), r).numerator
    }).toSeq.distinct

    println("reevaluateAllPolyhedra found differences: " + differences.toMathematicaInputString)

    differences.foldLeft(Seq(this))({ (s, d) => s.flatMap(r => r.declarePolynomialZero(d)) })
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
    lazy val determinant = Matrix(candidateIndependentDiagrams.size, matrix).determinant(rationalFunctions)
    lazy val determinantNumerator = determinant.ensuring(r => definitelyNonzero_?(r.denominator)).numerator

    val addIndependentDiagram: Option[SpiderData] = {
      if (candidateIndependentDiagrams.size <= dimensionBounds(boundary)) {
        val newIndependentDiagrams = paddedIndependentDiagrams.updated(boundary, candidateIndependentDiagrams)
        declarePolynomialNonzero(determinantNumerator).map(_.copy(
          consideredDiagrams = newConsideredDiagrams,
          independentDiagrams = newIndependentDiagrams))
      } else {
        None
      }
    }
    val addDependentDiagram: Seq[SpiderData] = {
      val nullSpace = Matrix(oldIndependentDiagrams.size + 1, rectangularMatrix).nullSpace(rationalFunctions)

      println("matrix: ")
      println(rectangularMatrix.toMathematicaInputString)
      println("nullspace: ")
      println(nullSpace.toMathematicaInputString)

      if (nullSpace.size > 1) {
        // the old diagrams have become dependent!
        // note to self --- reevaluating all polyhedra whenever we add a reduction seems to avoid this possibility
        println("old diagrams must have become dependent")
        require(false)
        Seq.empty
      } else {

        val relation = nullSpace.ensuring(_.size == 1).head
        require(relation.last == rationalFunctions.one)

        // is it a reducing relation?
        val nonzeroPositions = relation.dropRight(1).zipWithIndex.collect({ case (x, i) if x != rationalFunctions.zero => i })
        val reducing = nonzeroPositions.nonEmpty && nonzeroPositions.forall({ i => complexity.lt(oldIndependentDiagrams(i), p) })

        if (reducing) {
          // are there denominators? we better ensure they are invertible
          val denominatorLCM = polynomials.lcm(relation.map(_.denominator): _*)

          val whenDenominatorsVanish = declarePolynomialZero(denominatorLCM).toSeq.flatMap(_.considerDiagram(p))

          require(p.numberOfInternalVertices > 0)
          val newReduction = Reduction(p, oldIndependentDiagrams.zip(relation.dropRight(1).map(rationalFunctions.negate)).toMap)
          val whenDenominatorsNonzero = declarePolynomialNonzero(denominatorLCM).toSeq.flatMap(
            _.copy(spider = spider.addReduction(
              newReduction)).reevaluateAllPolyhedra)

          whenDenominatorsVanish ++ whenDenominatorsNonzero
        } else {
          // hmm... just ask that the determinant vanishes
          // TODO record non-reducing relations!
          declarePolynomialZero(determinantNumerator).toSeq.map(_.copy(consideredDiagrams = newConsideredDiagrams))
        }
      }
    }

    addIndependentDiagram.toSeq ++ addDependentDiagram
  }

  def normalizePolynomial(p: MultivariablePolynomial[Fraction[BigInt], String]) = {
    def bigRationals = implicitly[Field[Fraction[BigInt]]]
    val a = p.coefficients(polynomials.leadingMonomial(p).get)
    polynomials.scalarMultiply(bigRationals.inverse(a), p)
  }
  
  def definitelyNonzero_?(p: MultivariablePolynomial[Fraction[BigInt], String]): Boolean = {
    import mathematica.Factor._
    val factors = p.factor.keys.map(normalizePolynomial)
    for (f <- factors.filterNot(f => f == polynomials.one || nonzero.contains(f))) println("factor which ought to be zero: " + f.toMathematicaInputString)
    factors.forall(f => f == polynomials.one || nonzero.contains(f))
  }

  def declarePolynomialNonzero(p: MultivariablePolynomial[Fraction[BigInt], String]): Option[SpiderData] = {
    if (p != polynomials.zero) {
      val newNonzero = {
        import mathematica.Factor._
        val factors = p.factor.keys.map(normalizePolynomial).filterNot(_ == polynomials.one)
        (nonzero ++ factors).distinct
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
      println("Computing Groebner basis for " + (groebnerBasis :+ r).toMathematicaInputString)
      (groebnerBasis :+ r).computeGroebnerBasis
    }

    // has everything collapsed?
    if (newGroebnerBasis.contains(polynomials.one)) {
      None
    } else {
      // does this kill anything in nonzero? 
      val newNonzero = nonzero.map(MultivariablePolynomialAlgebras.quotient(newGroebnerBasis).normalForm).filter(_ != polynomials.one)
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
    nonzero = Seq(MultivariablePolynomial(Map(Map("p1" -> 1) -> Fraction[BigInt](1, 1)))),
    Seq.empty,
    dimensionBounds = Seq(1, 0, 1, 0, 3),
    Seq.empty,
    Seq.empty,
    Seq.empty)

  val steps = Seq((0, 0), (2, 0), (0, 1), (0, 2), (2, 1), (2, 2), (4, 0), (4, 1), (4, 2))

  // TODO nonzero shouldn't ever contain -1
  // TODO verify that all the spiders at this point have reductions for R2
  // TODO start computing relations, also
  // TODO simplify reductions, by discarding old reductions that are consequences of new ones

  val results = initialData.considerDiagrams(steps)

  println(results.size)
}