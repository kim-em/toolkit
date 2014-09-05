package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.mathematica._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.collections.KSubsets
import net.tqft.toolkit.algebra.matrices.Matrix
import MathematicaForm._
import net.tqft.toolkit.Logging

case class SpiderData(
  spider: QuotientSpider,
  groebnerBasis: Seq[MultivariablePolynomial[Fraction[BigInt], String]],
  nonReducingRelations: Seq[Seq[Seq[MultivariablePolynomial[Fraction[BigInt], String]]]], // given as linear combinations of the considered diagrams
  dimensionBounds: Seq[Int],
  consideredDiagramsVertexBound: Seq[Int],
  consideredDiagrams: Seq[Seq[PlanarGraph]],
  independentDiagrams: Seq[Seq[PlanarGraph]],
  visiblyIndependentDiagrams: Seq[Seq[PlanarGraph]]) {

  // verify
  def verify {
    for ((n, (k, j)) <- dimensionBounds.zip(independentDiagrams.map(_.size).zip(visiblyIndependentDiagrams.map(_.size)))) {
      require(k <= n)
      require(j >= 2 * k - n)
    }

    for (visiblyIndependent <- visiblyIndependentDiagrams) {
      // the determinants of visibly independent vectors must be invertible
      val det = calculateDeterminant(visiblyIndependent)
      require(declarePolynomialZero(det).isEmpty)
    }
  }

  type P = MultivariablePolynomial[Fraction[BigInt], String]

  override def toString = {
    s"""SpiderData(
  groebnerBasis = ${groebnerBasis.toMathematicaInputString},
  dimensionBounds = $dimensionBounds,
  independentDiagrams.sizes: ${independentDiagrams.map(_.size)},
  visiblyIndependentDiagrams.sizes: ${visiblyIndependentDiagrams.map(_.size)},
  consideredDiagrams.sizes: ${consideredDiagrams.map(_.size)},
  allPolyhedra: $allPolyhedra,
  reducibleDiagrams: ${spider.extraReductions.map(_.big)},
  consideredDiagramsVertexBound = $consideredDiagramsVertexBound
)"""
  }

  def allPolyhedra: Seq[String] =
    (groebnerBasis.flatMap(_.variables) ++
      spider.reductions.flatMap(_.small.values.flatMap(r => r.variables)) ++
      nonReducingRelations.flatMap(_.flatMap(_.flatMap(_.variables)))).distinct.filter(!_.contains("^(-1)"))

  val complexity: Ordering[PlanarGraph] = {
    Ordering.by(_.numberOfInternalVertices)
  }

  // This forces all the formal inverses to come last in the variable ordering, which seems to help the Groebner bases a bit.
  implicit object stringOrdering extends Ordering[String] {
    override def compare(x: String, y: String) = {
      if (x.endsWith("^(-1)") && !y.endsWith("^(-1)")) {
        1
      } else if (!x.endsWith("^(-1)") && y.endsWith("^(-1)")) {
        -1
      } else {
        Ordering.String.compare(x, y)
      }
    }
  }

  lazy val polynomials = MultivariablePolynomialAlgebras.quotient(groebnerBasis)
  lazy val rationalFunctions = Field.fieldOfFractions(polynomials)

  def addReduction(r: Reduction[PlanarGraph, MultivariablePolynomial[Fraction[BigInt], String]]): Seq[SpiderData] = {
    val newSpider = spider.addReduction(r)

    def reevaluateAllPolyhedra: Seq[SpiderData] = {
      val differences = (for (p <- allPolyhedra.iterator; g <- PolyhedronNamer.byName(p).iterator; r <- newSpider.allEvaluations(g)) yield {
        rationalFunctions.subtract(Fraction.whole(polynomials.monomial(p)), r).numerator
      }).toSeq.distinct

      println("reevaluateAllPolyhedra found differences: " + differences.toMathematicaInputString)

      differences.foldLeft(Seq(this))({ (s, d) => s.flatMap(r => r.declarePolynomialZero(d)) })
    }
    reevaluateAllPolyhedra.map(_.copy(spider = newSpider).simplifyReductions)
  }

  def simplifyReductions: SpiderData = {
    // FIXME I'm suspicious of this!

    spider.extraReductions.tails.find(tail => tail.nonEmpty && tail.tail.find(d => tail.head.big.Subgraphs(d.big).excisions.nonEmpty).nonEmpty) match {
      case Some(tail) => {
        Logging.info("Discarding a redundant reduction for " + tail.head.big)
        copy(spider = spider.copy(extraReductions = spider.extraReductions.filterNot(_.big == tail.head.big))).simplifyReductions
      }
      case None => this
    }
  }

  def innerProducts(diagrams1: Seq[PlanarGraph], diagrams2: Seq[PlanarGraph]): Seq[Seq[MultivariablePolynomial[Fraction[BigInt], String]]] = {
    spider.innerProductMatrix(diagrams1, diagrams2).map(row => row.map(entry => polynomials.normalForm(entry)))
  }

  def innerProducts(diagrams: Seq[PlanarGraph]): Seq[Seq[MultivariablePolynomial[Fraction[BigInt], String]]] = innerProducts(diagrams, diagrams)

  def calculateDeterminant(diagrams: Seq[PlanarGraph]) = {
    val matrix = innerProducts(diagrams)
    println("computing determinant of: ")
    println(matrix.toMathematicaInputString)

    import mathematica.Determinant.ofMultivariablePolynomialMatrix._
    val result = polynomials.normalForm(matrix.cachedDeterminant)

    println("determinant: ")
    println(result.toMathematicaInputString)

    result
  }

  def considerDiagram(p: PlanarGraph): Seq[SpiderData] = {
    if (spider.reducibleDiagram_?(p)) {
      println("ignoring a diagram that is now reducible: " + p)
      Seq(this)
    } else {
      println("considering diagram: " + p)
      println("for: " + this)

      val boundary = p.numberOfBoundaryPoints
      val newConsideredDiagrams = {
        val padded = consideredDiagrams.padTo(boundary + 1, Seq.empty)
        padded.updated(boundary, (padded(boundary) :+ p).distinct)
      }
      val paddedIndependentDiagrams = independentDiagrams.padTo(boundary + 1, Seq.empty)
      val paddedVisiblyIndependentDiagrams = visiblyIndependentDiagrams.padTo(boundary + 1, Seq.empty)
      val paddedNonReducingRelations = nonReducingRelations.padTo(boundary + 1, Seq.empty)
      val padded = copy(independentDiagrams = paddedIndependentDiagrams, visiblyIndependentDiagrams = paddedVisiblyIndependentDiagrams, nonReducingRelations = paddedNonReducingRelations, consideredDiagrams = newConsideredDiagrams)

      val addDependent = padded.addDependentDiagram(p)
      val addIndependent = if (padded.independentDiagrams(boundary).size < dimensionBounds(boundary)) {
        padded.addIndependentDiagram(p)
      } else {
        Seq.empty
      }

      addDependent ++ addIndependent
    }
  }

  def addDependentDiagram(p: PlanarGraph): Seq[SpiderData] = {
    println("adding a dependent diagram: " + p)

    val rectangularMatrix = innerProducts(visiblyIndependentDiagrams(p.numberOfBoundaryPoints), independentDiagrams(p.numberOfBoundaryPoints) :+ p).map(row => row.map(entry => Fraction.whole(entry)))

    println("computing nullspace of: ")
    println(rectangularMatrix.toMathematicaInputString)
    import mathematica.NullSpace.ofMultivariableRationalFunctionMatrix._

    val nullSpace = {
      if (independentDiagrams(p.numberOfBoundaryPoints).size == 0) {
        Seq(Seq(rationalFunctions.one))
      } else {
        rectangularMatrix.nullSpace
      }
    }

    println("nullspace: ")
    println(nullSpace.toMathematicaInputString)

    val relation = nullSpace.ensuring(_.size == independentDiagrams(p.numberOfBoundaryPoints).size - visiblyIndependentDiagrams(p.numberOfBoundaryPoints).size + 1).head
    require(relation.last == rationalFunctions.one)

    // are there denominators? we better ensure they are invertible
    val denominators = relation.map(_.denominator)

    val whenDenominatorsVanish = declareAtLeastOnePolynomialZero(denominators).flatMap(_.addDependentDiagram(p))
    val whenDenominatorsNonzero = for (
      (s, _) <- invertPolynomials(denominators).toSeq;
      liftedRelation = relation.map(s.liftDenominators);
      t <- addRelation(p.numberOfBoundaryPoints, (independentDiagrams(p.numberOfBoundaryPoints) :+ p).zip(liftedRelation))
    ) yield t

    whenDenominatorsVanish ++ whenDenominatorsNonzero
  }

  def addRelations(size: Int, relations: Seq[Seq[(PlanarGraph, MultivariablePolynomial[Fraction[BigInt], String])]]): Seq[SpiderData] = {
    relations.foldLeft(Seq(this))({ (spiders, relation) => spiders.flatMap(s => s.addRelation(size, relation)) })
  }

  def addRelation(size: Int, relation: Seq[(PlanarGraph, MultivariablePolynomial[Fraction[BigInt], String])]): Seq[SpiderData] = {
    // TODO remove these checks
    for (tail <- relation.map(_._1).tails; if tail.nonEmpty) {
      for (y <- tail.tail) {
        require(tail.head.canonicalFormWithDefect._1 != y.canonicalFormWithDefect._1)
      }
    }
    addRelations(size - 2, for (
      i <- 0 until size;
      c = spider.stitchAt(relation.toMap, i);
      if !spider.zero_?(c)
    ) yield c.toSeq).flatMap(_._addRelation(size, relation))
  }

  private def _addRelation(size: Int, relation0: Seq[(PlanarGraph, MultivariablePolynomial[Fraction[BigInt], String])]): Seq[SpiderData] = {
    val relation = relation0.map(p => (p._1, polynomials.normalForm(p._2))).filter(p => !polynomials.zero_?(p._2))
    if (relation.size == 0) {
      Seq(this)
    } else if (relation.size == 1 && size == 0) {
      if (relation.head._1 == PlanarGraph.empty) {
        declarePolynomialZero(relation.head._2)
      } else {
        _addRelation(0, Seq((PlanarGraph.empty, spider.evaluate(relation.toMap))))
      }
    } else {

      val p = relation.last._1
      val px = relation.last._2

      if (px != polynomials.one) {
        ((for (s <- declarePolynomialZero(px)) yield {
          require(s.polynomials.zero_?(px))
          val newRelation = relation.map(q => (q._1, s.polynomials.normalForm(q._2))).filter(q => !s.polynomials.zero_?(q._2))
          require(newRelation.size < relation.size)
          s._addRelation(size, newRelation)
        }) ++
          (for ((s, i) <- invertPolynomial(px)) yield {
            s._addRelation(size, relation.map(q => (q._1, s.polynomials.multiply(q._2, i))))
          })).flatten
      } else {
        val pairings =
          for (i <- 0 until size) yield {
            spider.evaluate(spider.multiply(relation.toMap, spider.rotate(Map(p -> polynomials.one), i), size))
          }

        // is it a reducing relation?
        val nonzeroPositions = relation.dropRight(1).zipWithIndex.collect({ case ((d, x), i) if !polynomials.zero_?(x) => i })
        val reducing = relation.size > 1 && nonzeroPositions.forall({ i => complexity.lt(relation(i)._1, relation.last._1) })
        println(s"reducing: $reducing")

        // There's a lemma here. If b \in span{a1, ..., an}, and {a1, ..., ak} is maximally visibly independent,
        // then the inner product determinant for {a1, ..., ak, b} vanishes.
        (for (
          s <- declareAllPolynomialsZero(pairings) //;
        //      s <- s0.declarePolynomialZero(calculateDeterminant(relation.map(_._1)))
        ) yield {

          if (reducing) {
            /* we should never run into reducing relations which contain an arc */
            if (relation.last._1.vertexFlags.head.map(_._1).distinct.size != size) {
              println("found a reducing relation that contains an arc: ")
              println(this)
              for ((d, z) <- relation) {
                println(d)
                println(" " + z)
              }
              ???
            }

            val newReduction = Reduction(p, relation.dropRight(1).map(q => (q._1, s.polynomials.negate(q._2))).toMap)
            s.addReduction(newReduction)
          } else {
            // record non-reducing relations
            val coefficients = {
              var found = 0
              val result = Seq.tabulate(consideredDiagrams(size).size)({ k =>
                val option = relation.find(_._1 == consideredDiagrams(size)(k))
                option match {
                  case Some((d, x)) => {
                    found = found + 1
                    x
                  }
                  case None => polynomials.zero
                }
              })
              require(found == relation.size)
              result
            }
            // TODO row-reduce them! (safely; keeping track of denominators)
            Seq(s.copy(nonReducingRelations = nonReducingRelations.updated(size, nonReducingRelations(size) :+ coefficients)))
          }
        }).flatten
      }
    }
  }

  def addIndependentDiagram(p: PlanarGraph): Seq[SpiderData] = {
    println(" adding an independent diagram: " + p)

    val newIndependentDiagrams = independentDiagrams.updated(p.numberOfBoundaryPoints, independentDiagrams(p.numberOfBoundaryPoints) :+ p)

    val addVisibly = addVisiblyIndependentDiagram(p).filter({ s =>
      s.visiblyIndependentDiagrams(p.numberOfBoundaryPoints).size >= 2 * independentDiagrams(p.numberOfBoundaryPoints).size + 2 - dimensionBounds(p.numberOfBoundaryPoints)
    })
    val addInvisibly = if (visiblyIndependentDiagrams(p.numberOfBoundaryPoints).size >= 2 * independentDiagrams(p.numberOfBoundaryPoints).size + 2 - dimensionBounds(p.numberOfBoundaryPoints)) {
      addInvisiblyIndependentDiagram(p)
    } else {
      Seq.empty
    }

    (addVisibly ++ addInvisibly).map(_.copy(independentDiagrams = newIndependentDiagrams))
  }

  def addVisiblyIndependentDiagram(p: PlanarGraph): Seq[SpiderData] = {
    println("adding a visibly independent diagram: " + p)

    val invisibleDiagrams = independentDiagrams(p.numberOfBoundaryPoints).filterNot(visiblyIndependentDiagrams(p.numberOfBoundaryPoints).contains)
    println("there are currently " + invisibleDiagrams.size + " invisible diagrams")
    val invisibleSubsets = {
      import net.tqft.toolkit.collections.Subsets._
      invisibleDiagrams.subsets.map(_.toSeq).toSeq.ensuring(s => s.size == 1 << invisibleDiagrams.size)
    }
    println("there are " + invisibleSubsets.size + " subsets of formerly invisible diagrams, which we need to reconsider.")
    val determinants = {
      (for (s <- invisibleSubsets) yield {
        println("considering a subset of size " + s.size)
        s -> calculateDeterminant(visiblyIndependentDiagrams(p.numberOfBoundaryPoints) ++ s :+ p)
      }).toMap
    }

    val determinantsWithBiggerDeterminants = {
      (for (s <- invisibleSubsets) yield {
        s -> (determinants(s), invisibleSubsets.filter(t => t.size > s.size && s.forall(t.contains)).map(determinants))
      })
    }

    (for ((s, (nonzero, allZero)) <- determinantsWithBiggerDeterminants) yield {
      val newVisiblyIndependentDiagrams = visiblyIndependentDiagrams.updated(p.numberOfBoundaryPoints, visiblyIndependentDiagrams(p.numberOfBoundaryPoints) ++ s :+ p)

      invertPolynomial(nonzero).map(_._1).toSeq.flatMap(_.declareAllPolynomialsZero(allZero)).map(_.copy(visiblyIndependentDiagrams = newVisiblyIndependentDiagrams))
    }).flatten
  }

  def addInvisiblyIndependentDiagram(p: PlanarGraph): Seq[SpiderData] = {
    println("adding an invisibly independent diagram: " + p)

    val invisibleDiagrams = independentDiagrams(p.numberOfBoundaryPoints).filterNot(visiblyIndependentDiagrams(p.numberOfBoundaryPoints).contains)
    val invisibleSubsets = {
      import net.tqft.toolkit.collections.Subsets._
      invisibleDiagrams.subsets.map(_.toSeq).toSeq
    }
    val determinants = {
      for (s <- invisibleSubsets) yield {
        calculateDeterminant(independentDiagrams(p.numberOfBoundaryPoints) ++ s :+ p)
      }
    }

    declareAllPolynomialsZero(determinants)
  }

  def normalizePolynomial(p: P) = {
    def bigRationals = implicitly[Field[Fraction[BigInt]]]
    polynomials.leadingMonomial(p) match {
      case Some(lm) => polynomials.scalarMultiply(bigRationals.inverse(p.coefficients(lm)), p)
      case None => polynomials.zero
    }

  }

  def liftDenominators(p: MultivariableRationalFunction[Fraction[BigInt], String]): P = polynomials.multiply(p.numerator, invertPolynomial(p.denominator).get._2)

  def invertPolynomial(p: P): Option[(SpiderData, P)] = {
    import mathematica.Factor._
    println("Factoring something we're inverting: " + p.toMathematicaInputString)
    val factors = p.factor
    println("Factors: ")
    for (f <- factors) println(f._1.toMathematicaInputString + "   ^" + f._2)

    val result = factors.foldLeft[Option[(SpiderData, P)]](Some(this, polynomials.one))({ (s: Option[(SpiderData, P)], q: (P, Int)) =>
      s.flatMap({
        z: (SpiderData, P) =>
          if (q._2 > 0) {
            z._1.invertIrreduciblePolynomial(q._1).map({ w =>
              (w._1, polynomials.multiply(polynomials.power(w._2, q._2), z._2))
            })
          } else {
            Some((z._1, polynomials.multiply(polynomials.power(q._1, -q._2), z._2)))
          }
      })
    })

    if (polynomials.totalDegree(p) > 0) {
      require(result.isEmpty || result.get._1.groebnerBasis.nonEmpty)
    }

    result
  }

  def invertPolynomials(ps: Seq[P]): Option[(SpiderData, Seq[P])] = {
    ps.foldLeft[Option[(SpiderData, Seq[P])]](Some((this, Seq.empty)))({ (o, p) => o.flatMap(q => q._1.invertPolynomial(p).map(r => (r._1, q._2 :+ r._2))) })
  }

  def inverse(p: P): P = {
    if (polynomials.totalDegree(p) == 0) {
      polynomials.ring.inverse(polynomials.constantTerm(p))
    } else {
      p.toMathematicaInputString match {
        case s if s.startsWith("(") && s.endsWith("^(-1)") => polynomials.monomial(s.stripPrefix("(").stripSuffix("^(-1)"))
        case s if s.contains("^(-1)") => ???
        case s => polynomials.monomial("(" + s + ")^(-1)")
      }
    }
  }
  def invertIrreduciblePolynomial(p: P): Option[(SpiderData, P)] = {
    println("inverting " + p.toMathematicaInputString)
    if (polynomials.zero_?(p)) {
      None
    } else {
      val i = inverse(p)
      declareIrreduciblePolynomialZero(polynomials.subtract(polynomials.multiply(i, p), polynomials.one)).map(s => (s, i))
    }
  }

  def declareAtLeastOnePolynomialZero(rs: Seq[P]): Seq[SpiderData] = {
    rs.flatMap(declarePolynomialZero)
  }

  def declareAllPolynomialsZero(rs: Seq[P]): Seq[SpiderData] = {
    rs.foldLeft(Seq(this))({ (data: Seq[SpiderData], r: P) => data.flatMap(_.declarePolynomialZero(r)) })
  }

  def declarePolynomialZero(r: P): Seq[SpiderData] = {
    if (polynomials.zero_?(r)) {
      Seq(this)
    } else if (r == polynomials.one || r == polynomials.negativeOne) {
      Seq.empty
    } else {
      val factors = {
        import mathematica.Factor._
        println("Factoring something we're going to set to zero: " + r.toMathematicaInputString)
        normalizePolynomial(r).factor.filter(_._2 > 0).keys.toSeq
      }
      val result = factors.flatMap(f => { require(!f.toMathematicaInputString.contains("^(-1)")); declareIrreduciblePolynomialZero(f) })
      for (s <- result) {
        require(s.polynomials.zero_?(r))
      }
      result
    }
  }

  def declareIrreduciblePolynomialZero(r: P): Option[SpiderData] = {
    if (polynomials.zero_?(r)) {
      Some(this)
    } else {
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
        val newPolynomials = MultivariablePolynomialAlgebras.quotient(newGroebnerBasis)
        // TODO if we have non-reducing relations, these will have to be updated as well
        val newExtraReductions = spider.extraReductions.map({
          case Reduction(big, small) => Reduction(big, small.map({ p => (p._1, newPolynomials.normalForm(p._2)) }))
        })
        val result = Some(copy(
          spider = spider.copy(extraReductions = newExtraReductions),
          groebnerBasis = newGroebnerBasis))
        for (s <- result) {
          require(s.polynomials.zero_?(r))
        }
        result
      }
    }
  }

  def considerDiagrams(boundary: Int, vertices: Int): Seq[SpiderData] = {
    println(s"Considering diagrams with $boundary boundary points and $vertices vertices...")

    val newConsideredDiagramsVertexBound = consideredDiagramsVertexBound.padTo(boundary + 1, 0).updated(boundary, vertices)
    val diagramsToConsider = spider.reducedDiagrams(boundary, vertices)
    for (d <- diagramsToConsider) println("   " + d)

    // FIXME during this fold, we might discover new reducing relations, after which some of these reduced diagrams are no longer reduced!

    diagramsToConsider.foldLeft(Seq(this))({
      (s: Seq[SpiderData], p: PlanarGraph) =>
        s.flatMap(d => d.considerDiagram(p))
    })
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

