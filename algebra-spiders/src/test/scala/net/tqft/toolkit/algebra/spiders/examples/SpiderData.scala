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
  independentDiagrams: Seq[Seq[PlanarGraph]],
  visiblyIndependentDiagrams: Seq[Seq[PlanarGraph]]) {

  require(!nonzero.contains(polynomials.one))

  override def toString = {
    s"""SpiderData(
  groebnerBasis = ${groebnerBasis.toMathematicaInputString},
  nonzero = ${nonzero.toMathematicaInputString},
  dimensionBounds = $dimensionBounds,
  dimensionLowerBounds: $dimensionLowerBounds,
  numberOfDiagramsConsidered: ${consideredDiagrams.map(_.size)},
  allPolyhedra: $allPolyhedra,
  reducibleDiagrams: ${spider.extraReductions.map(_.big)},
  consideredDiagramsVertexBound = $consideredDiagramsVertexBound
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

  def addReduction(r: Reduction[PlanarGraph, MultivariableRationalFunction[Fraction[BigInt], String]]): Seq[SpiderData] = {
    copy(spider = spider.addReduction(r)).reevaluateAllPolyhedra.map(_.simplifyReductions)
  }

  def simplifyReductions: SpiderData = {
    spider.extraReductions.tails.find(tail => tail.nonEmpty && tail.tail.find(d => tail.head.big.Subgraphs(d.big).excisions.nonEmpty).nonEmpty) match {
      case Some(tail) => copy(spider = spider.copy(extraReductions = spider.extraReductions.filterNot(_.big == tail.head.big))).simplifyReductions
      case None => this
    }
  }

  def calculateDeterminant(diagrams: Seq[PlanarGraph]) = {
    val matrix = spider.innerProductMatrix(diagrams, diagrams).map(row => row.map(entry => Fraction(polynomials.normalForm(entry.numerator), polynomials.normalForm(entry.denominator))))
    
    println("computing determinant of: ")
    println(matrix.toMathematicaInputString)

    import mathematica.Determinant.ofMultivariableRationalFunctionMatrix._
    val d = matrix.determinant
    val result = Fraction(polynomials.normalForm(d.numerator), polynomials.normalForm(d.denominator))

    println("determinant: ")
    println(result.toMathematicaInputString)
    
    result
  }

  def considerDiagram(p: PlanarGraph): Seq[SpiderData] = {
    addDependentDiagram(p) ++ addIndependentDiagram(p)
  }

  def addDependentDiagram(p: PlanarGraph): Seq[SpiderData] = {
    ???
  }

  def addIndependentDiagram(p: PlanarGraph): Seq[SpiderData] = {
    addVisiblyIndependentDiagram(p) ++ addInvisiblyIndependentDiagram(p)
  }

  def addVisiblyIndependentDiagram(p: PlanarGraph): Seq[SpiderData] = {
    ???
  }

  def addInvisiblyIndependentDiagram(p: PlanarGraph): Seq[SpiderData] = {
    ???
  }

  def oldConsiderDiagram(p: PlanarGraph): Seq[SpiderData] = {
    val boundary = p.numberOfBoundaryPoints
    val newConsideredDiagrams = {
      val padded = consideredDiagrams.padTo(boundary + 1, Seq.empty)
      padded.updated(boundary, (padded(boundary) :+ p).distinct)
    }
    val paddedIndependentDiagrams = independentDiagrams.padTo(boundary + 1, Seq.empty)
    val paddedVisiblyIndependentDiagrams = visiblyIndependentDiagrams.padTo(boundary + 1, Seq.empty)
    val oldIndependentDiagrams = paddedIndependentDiagrams(boundary)
    val oldVisiblyIndependentDiagrams = paddedVisiblyIndependentDiagrams(boundary)
    val candidateIndependentDiagrams = oldIndependentDiagrams :+ p

    // FIXME we can save more inner products; individual spiders come and go, but inner products survive.
    // FIXME the spider should know the ring, so the inner product automatically happens mod the ideal.
    lazy val shortMatrix = spider.innerProductMatrix(oldVisiblyIndependentDiagrams, candidateIndependentDiagrams).map(row => row.map(entry => Fraction(polynomials.normalForm(entry.numerator), polynomials.normalForm(entry.denominator))))
    lazy val rectangularMatrix = spider.innerProductMatrix(oldIndependentDiagrams, candidateIndependentDiagrams).map(row => row.map(entry => Fraction(polynomials.normalForm(entry.numerator), polynomials.normalForm(entry.denominator))))
    lazy val lastRow = spider.innerProductMatrix(Seq(p), candidateIndependentDiagrams).map(row => row.map(entry => Fraction(polynomials.normalForm(entry.numerator), polynomials.normalForm(entry.denominator))))
    def matrix = rectangularMatrix ++ lastRow
    lazy val determinant = {
      println("computing determinant of: ")
      println(matrix.toMathematicaInputString)

      import mathematica.Determinant.ofMultivariableRationalFunctionMatrix._
      val d = matrix.determinant
      val result = Fraction(polynomials.normalForm(d.numerator), polynomials.normalForm(d.denominator))

      println("determinant: ")
      println(result.toMathematicaInputString)
      result

    }
    lazy val determinantNumerator = determinant /*.ensuring(r => definitelyNonzero_?(r.denominator))*/ .numerator

    val addIndependentDiagram: Seq[SpiderData] = {
      println("adding independent diagram: " + p)

      val invisibleDiagrams = oldIndependentDiagrams.filterNot(oldVisiblyIndependentDiagrams.contains)
      val invisibleSubsets = {
        import net.tqft.toolkit.collections.Subsets._
        invisibleDiagrams.subsets.map(_.toSeq).toSeq
      }
      val determinants = {

        (for (s <- invisibleSubsets) yield {
          s -> calculateDeterminant(oldIndependentDiagrams ++ s :+ p)
        }).toMap
      }
      val determinantsAndBiggerDeterminants = {
        (for (s <- invisibleSubsets) yield {
          s -> (determinants(s), invisibleSubsets.filter(t => t.size > s.size && s.forall(t.contains)).map(determinants))
        }).toMap
      }

      for ((s, (nonzero, allZero)) <- determinantsAndBiggerDeterminants) yield {
        val newIndependentDiagrams = paddedIndependentDiagrams.updated(boundary, candidateIndependentDiagrams)
        val newVisiblyIndependentDiagrams = paddedVisiblyIndependentDiagrams.updated(boundary, oldVisiblyIndependentDiagrams ++ s :+ p)

      }

      if (candidateIndependentDiagrams.size <= dimensionBounds(boundary)) {
        val newIndependentDiagrams = paddedIndependentDiagrams.updated(boundary, candidateIndependentDiagrams)
        val newVisiblyIndependentDiagrams = ???
        declarePolynomialNonzero(determinantNumerator).map(_.copy(
          dimensionBounds = dimensionBounds.updated(boundary, newIndependentDiagrams.size),
          consideredDiagrams = newConsideredDiagrams,
          independentDiagrams = newIndependentDiagrams,
          visiblyIndependentDiagrams = newVisiblyIndependentDiagrams)).toSeq ++
          Seq(copy(consideredDiagrams = newConsideredDiagrams,
            independentDiagrams = newIndependentDiagrams))
      } else {
        Seq.empty
      }
    }
    val addDependentDiagram: Seq[SpiderData] = {
      println("adding dependent diagram: " + p)
      println("computing nullspace of: ")
      println(rectangularMatrix.toMathematicaInputString)
      import mathematica.NullSpace.ofMultivariableRationalFunctionMatrix._

      //      val nullSpace = Matrix(oldIndependentDiagrams.size + 1, rectangularMatrix).nullSpace(rationalFunctions)
      val nullSpace = {
        if (oldIndependentDiagrams.size == 0) {
          Seq(Seq(rationalFunctions.one))
        } else {
          rectangularMatrix.nullSpace
        }
      }

      println("nullspace: ")
      println(nullSpace.toMathematicaInputString)

      val relation = nullSpace.ensuring(_.size == 1).head
      require(!rationalFunctions.zero_?(relation.last))
      require(relation.last == rationalFunctions.one)

      // is it a reducing relation?
      val nonzeroPositions = relation.dropRight(1).zipWithIndex.collect({ case (x, i) if x != rationalFunctions.zero => i })
      val reducing = relation.size > 1 && nonzeroPositions.forall({ i => complexity.lt(oldIndependentDiagrams(i), p) })
      println(s"reducing: $reducing")

      if (reducing && p.vertexFlags.head.map(_._1).distinct.size == p.numberOfBoundaryPoints /* a very annoying implementation restriction */ ) {
        // are there denominators? we better ensure they are invertible
        val denominatorLCM = polynomials.lcm(relation.map(_.denominator): _*)

        val whenDenominatorsVanish = declarePolynomialZero(denominatorLCM).toSeq.flatMap(_.considerDiagram(p))

        require(p.numberOfInternalVertices > 0)
        val newReduction = Reduction(p, oldIndependentDiagrams.zip(relation.dropRight(1).map(rationalFunctions.negate)).toMap)
        val whenDenominatorsNonzero = declarePolynomialNonzero(denominatorLCM).toSeq.flatMap(_.addReduction(newReduction)).map(_.copy(consideredDiagrams = newConsideredDiagrams))

        whenDenominatorsVanish ++ whenDenominatorsNonzero
      } else {
        // hmm... just ask that the determinant vanishes
        // TODO record non-reducing relations!
        declarePolynomialZero(determinantNumerator).toSeq.map(_.copy(consideredDiagrams = newConsideredDiagrams))
      }
    }

    addIndependentDiagram.toSeq ++ addDependentDiagram
  }

  def normalizePolynomial(p: MultivariablePolynomial[Fraction[BigInt], String]) = {
    def bigRationals = implicitly[Field[Fraction[BigInt]]]
    polynomials.leadingMonomial(p) match {
      case Some(lm) => polynomials.scalarMultiply(bigRationals.inverse(p.coefficients(lm)), p)
      case None => polynomials.zero
    }

  }

  //  def definitelyNonzero_?(p: MultivariablePolynomial[Fraction[BigInt], String]): Boolean = {
  //    import mathematica.Factor._
  //    val factors = p.factor.keys.map(normalizePolynomial)
  //    for (f <- factors.filterNot(f => f == polynomials.one || nonzero.contains(f))) println("factor which ought to be zero: " + f.toMathematicaInputString)
  //    factors.forall(f => f == polynomials.one || nonzero.contains(f))
  //  }

  def declarePolynomialNonzero(p: MultivariablePolynomial[Fraction[BigInt], String]): Option[SpiderData] = {
    if (!polynomials.zero_?(p)) {
      val newNonzero = {
        import mathematica.Factor._
        println("Factoring something we're insisting is nonzero: " + p.toMathematicaInputString)
        val factors = (p.factor.keySet + p).map(normalizePolynomial).filterNot(_ == polynomials.one)
        (nonzero ++ factors).distinct // we add both p and its factors to the list; Groebner basis reduction could kill p without killing any factors
      }
      Some(copy(nonzero = newNonzero))
    } else {
      println("polynomial was already zero; stopping")
      None
    }

  }

  def declareAtLeastOnePolynomialZero(rs: Seq[MultivariablePolynomial[Fraction[BigInt], String]]): Seq[SpiderData] = {
    rs.flatMap(declarePolynomialZero)
  }
  
  def declarePolynomialZero(r: MultivariablePolynomial[Fraction[BigInt], String]): Seq[SpiderData] = {
    if (polynomials.zero_?(r)) {
      Seq(this)
    } else {
      val factors = {
        import mathematica.Factor._
        println("Factoring something we're going to set to zero: " + r.toMathematicaInputString)
        normalizePolynomial(r).factor.keys.toSeq.ensuring(_.nonEmpty)
      }
      factors.flatMap(declareIrreduciblePolynomialZero)
    }
  }

  def declareIrreduciblePolynomialZero(r: MultivariablePolynomial[Fraction[BigInt], String]): Option[SpiderData] = {
    val newGroebnerBasis = {
      import mathematica.GroebnerBasis._
      println("Computing Groebner basis for " + (groebnerBasis :+ r).toMathematicaInputString)
      val result = (groebnerBasis :+ r).computeGroebnerBasis
      println(" ... result: " + result.toMathematicaInputString)
      result
    }

    // has everything collapsed?
    if (newGroebnerBasis.contains(polynomials.one)) {
      None
    } else {
      // does this kill anything in nonzero? 
      val newPolynomials = MultivariablePolynomialAlgebras.quotient(newGroebnerBasis)
      val newNonzero = nonzero.map(newPolynomials.normalForm).map(normalizePolynomial).filter(_ != polynomials.one).distinct
      if (newNonzero.exists(_ == polynomials.zero)) {
        None
      } else {
        // TODO if we have non-reducing relations, these will have to be updated as well
        val newExtraReductions = spider.extraReductions.map({
          case Reduction(big, small) => Reduction(big, small.map({ p => (p._1, Fraction(newPolynomials.normalForm(p._2.numerator), newPolynomials.normalForm(p._2.denominator))) }))
        })
        Some(copy(
          spider = spider.copy(extraReductions = newExtraReductions),
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

    diagramsToConsider.foldLeft(Seq(this))({ (s: Seq[SpiderData], p: PlanarGraph) => s.flatMap(d => d.considerDiagram(p)).filter(s => s.independentDiagrams.lift(6).getOrElse(Seq.empty).size == s.consideredDiagrams.lift(6).getOrElse(Seq.empty).size) })
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
    nonzero = Seq(
      MultivariablePolynomial(Map(Map("p1" -> 1) -> Fraction[BigInt](1, 1))),
      MultivariablePolynomial(Map(Map("p2" -> 1) -> Fraction[BigInt](1, 1))),
      MultivariablePolynomial(Map(Map() -> Fraction[BigInt](1, 1), Map("p1" -> 1) -> Fraction[BigInt](1, 1))),
      MultivariablePolynomial(Map(Map() -> Fraction[BigInt](-1, 1), Map("p1" -> 1) -> Fraction[BigInt](1, 1)))),
    Seq.empty,
    dimensionBounds = Seq(1, 0, 1, 0, 3, 0, 14),
    Seq.empty,
    Seq.empty,
    Seq.empty)

  val steps = Seq((0, 0), (2, 0), (0, 1), (0, 2), (2, 1), (2, 2), (4, 0), (4, 1), (4, 2), (6, 0), (6, 1), (6, 2))

  // TODO start computing relations, also

  val results = initialData.considerDiagrams(steps)

  println(results.size)
}