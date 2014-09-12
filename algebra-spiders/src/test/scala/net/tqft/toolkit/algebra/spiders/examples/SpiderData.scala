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
  nonReducingRelations: Seq[Seq[Seq[MultivariableRationalFunction[Fraction[BigInt], String]]]], // given as linear combinations of the considered diagrams
  dimensionBounds: Seq[Int],
  consideredDiagramsVertexBound: Seq[Int],
  consideredDiagrams: Seq[Seq[PlanarGraph]],
  independentDiagrams: Seq[Seq[PlanarGraph]],
  visiblyIndependentDiagrams: Seq[Seq[PlanarGraph]],
  observedPolyhedra: Set[String]) {

  val complexity: Ordering[PlanarGraph] = {
    Ordering.by(_.numberOfInternalVertices)
  }

  //    verify
  def verify {
    println(s"Beginning verification for $this")

    for ((n, (k, j)) <- dimensionBounds.zip(independentDiagrams.map(_.size).zip(visiblyIndependentDiagrams.map(_.size)))) {
      require(k <= n)
      require(j >= 2 * k - n)
    }

    for ((relations, k) <- nonReducingRelations.zipWithIndex; relation <- relations) {
      val i = relation.lastIndexWhere(x => !rationalFunctions.zero_?(x))
      require(consideredDiagrams(k).take(i).exists(d => complexity.gteq(d, consideredDiagrams(k)(i))))
    }
    for (visiblyIndependent <- visiblyIndependentDiagrams) {
      // the determinants of visibly independent vectors must be invertible
      val det = calculateDeterminant(visiblyIndependent)
      require(declarePolynomialZero(det.numerator).isEmpty)
    }
    for ((visiblyIndependent, independent) <- visiblyIndependentDiagrams.zip(independentDiagrams); D <- independent.toSet -- visiblyIndependent) {
      val det = calculateDeterminant(visiblyIndependent :+ D)
      require(rationalFunctions.zero_?(det))
    }
  }

  type P = MultivariablePolynomial[Fraction[BigInt], String]
  type R = MultivariableRationalFunction[Fraction[BigInt], String]

  override def toString = {
    s"""SpiderData(
  groebnerBasis = ${groebnerBasis.toMathematicaInputString},
  dimensionBounds = $dimensionBounds,
  independentDiagrams.sizes: ${independentDiagrams.map(_.size)},
  visiblyIndependentDiagrams.sizes: ${visiblyIndependentDiagrams.map(_.size)},
  consideredDiagrams.sizes: ${consideredDiagrams.map(_.size)},
  observedPolyhedra: $observedPolyhedra,
  reducibleDiagrams: ${spider.extraReductions.map(_.big)},
  consideredDiagramsVertexBound = $consideredDiagramsVertexBound
)"""
  }

  def allPolyhedra: Seq[String] =
    (groebnerBasis.flatMap(_.variables) ++
      spider.reductions.flatMap(_.small.values.flatMap(r => r.variables)) ++
      nonReducingRelations.flatMap(_.flatMap(_.flatMap(_.variables)))).distinct.filter(!_.contains("^(-1)"))

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
  lazy val rationalFunctions: Ring[R] = Field.fieldOfFractions(polynomials)

  private def normalForm(p: P): P = polynomials.normalForm(p)
  private def normalForm(r: R): R = Fraction(polynomials.normalForm(r.numerator), polynomials.normalForm(r.denominator))

  def allPolyhedronIdentities(s: String): Seq[R] = {
    def allPolyhedronReducingIdentities(s: String): Seq[R] = {
      for (g <- PolyhedronNamer.byName(s).toSeq; r <- spider.allEvaluations(g)) yield {
        rationalFunctions.subtract(Fraction.whole(polynomials.monomial(s)), r)
      }
    }
    def allPolyhedronNonReducingIdentities(s: String): Seq[R] = {
      (for (g <- PolyhedronNamer.byName(s).toSeq; (relations, k) <- nonReducingRelations.zipWithIndex; relation <- relations; diagrams = consideredDiagrams(k)) yield {
        require(relation.last == rationalFunctions.one)
        val bigDiagram = diagrams(relation.size - 1)
        for (e <- g.Subgraphs(bigDiagram).excisions) yield {
          rationalFunctions.sum(for ((x, d) <- relation.zip(diagrams)) yield {
            rationalFunctions.multiply(spider.evaluate(spider.replaceRepeatedly(spider.reductions)(Map(e.replace(d) -> rationalFunctions.one))), x)
          })
        }
      }).flatten
    }
    allPolyhedronReducingIdentities(s) ++ allPolyhedronNonReducingIdentities(s)
  }
  def observePolyhedron(s: String): Seq[SpiderData] = {
    copy(observedPolyhedra = observedPolyhedra + s).declareAllPolynomialsZero(allPolyhedronIdentities(s).map(_.numerator))
  }
  def observePolyhedra(ss: Set[String]): Seq[SpiderData] = {
    ss.foldLeft(Seq(this))({ (p: Seq[SpiderData], s: String) => p.flatMap(_.observePolyhedron(s)) })
  }

  def addReduction(r: Reduction[PlanarGraph, R]): Seq[SpiderData] = {
    copy(spider = spider.addReduction(r)).observePolyhedra(allPolyhedra.toSet).map(_.simplifyReductions)
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

  def innerProducts(diagrams1: Seq[PlanarGraph], diagrams2: Seq[PlanarGraph]): Seq[Seq[R]] = {
    spider.innerProductMatrix(diagrams1, diagrams2).map(row => row.map(entry => normalForm(entry)))
  }

  def innerProducts(diagrams: Seq[PlanarGraph]): Seq[Seq[R]] = innerProducts(diagrams, diagrams)

  def calculateDeterminant(diagrams: Seq[PlanarGraph]) = {
    val matrix = innerProducts(diagrams)
    println("computing determinant of: ")
    println(matrix.toMathematicaInputString)

    import mathematica.Determinant.ofMultivariableRationalFunctionMatrix._
    val result = normalForm(matrix.cachedDeterminant)

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

      val pairingsWithNonReducingRelations = for (r <- nonReducingRelations.applyOrElse(boundary, { i: Int => Seq.empty })) yield {
        normalForm(spider.evaluate(spider.innerProduct(consideredDiagrams(boundary).zip(r).toMap, Map(p -> polynomials.one)))).numerator
      }

      if (pairingsWithNonReducingRelations.forall(polynomials.zero_?)) {
        (for (s <- observePolyhedra(polynomials.variables(spider.evaluate(spider.innerProduct(Map(p -> polynomials.one), Map(p -> polynomials.one))).numerator))) yield {
          val newConsideredDiagrams = {
            val padded = s.consideredDiagrams.padTo(boundary + 1, Seq.empty)
            padded.updated(boundary, (padded(boundary) :+ p).distinct)
          }
          val paddedIndependentDiagrams = s.independentDiagrams.padTo(boundary + 1, Seq.empty)
          val paddedVisiblyIndependentDiagrams = s.visiblyIndependentDiagrams.padTo(boundary + 1, Seq.empty)
          val paddedNonReducingRelations = s.nonReducingRelations.padTo(boundary + 1, Seq.empty)
          val padded = s.copy(independentDiagrams = paddedIndependentDiagrams, visiblyIndependentDiagrams = paddedVisiblyIndependentDiagrams, nonReducingRelations = paddedNonReducingRelations, consideredDiagrams = newConsideredDiagrams)

          val addDependent = padded.addDependentDiagram(p)
          val addIndependent = if (padded.independentDiagrams(boundary).size < dimensionBounds(boundary)) {
            padded.addIndependentDiagram(p)
          } else {
            Seq.empty
          }
          addDependent ++ addIndependent
        }).flatten

      } else {
        declareAllPolynomialsZero(pairingsWithNonReducingRelations).flatMap(_.considerDiagram(p))
      }
    }
  }

  def addDependentDiagram(p: PlanarGraph): Seq[SpiderData] = {
    println("adding a dependent diagram: " + p)

    val rectangularMatrix = innerProducts(visiblyIndependentDiagrams(p.numberOfBoundaryPoints), independentDiagrams(p.numberOfBoundaryPoints) :+ p)

    println("computing nullspace of: ")
    println(rectangularMatrix.toMathematicaInputString)
    import mathematica.NullSpace.ofMultivariableRationalFunctionMatrix._

    val nullSpace = {
      if (visiblyIndependentDiagrams(p.numberOfBoundaryPoints).size == 0) {
        Seq.tabulate(independentDiagrams(p.numberOfBoundaryPoints).size + 1, independentDiagrams(p.numberOfBoundaryPoints).size + 1)({ case (i, j) => if (i == j) rationalFunctions.one else rationalFunctions.zero }).reverse
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

    if (!denominators.forall(p => !polynomials.zero_?(p))) {
      println("something went wrong!")
      println(this)
      println(p)
      println(independentDiagrams(p.numberOfBoundaryPoints))
      println(visiblyIndependentDiagrams(p.numberOfBoundaryPoints))
      require(false)
    }

    val whenDenominatorsVanish = declareAtLeastOnePolynomialZero(denominators).flatMap(_ /*.ensuring(_.groebnerBasis != groebnerBasis)*/ .addDependentDiagram(p))
    val whenDenominatorsNonzero = for (
      (s, _) <- invertPolynomials(denominators).toSeq;
      //      liftedRelation = relation.map(s.liftDenominators);
      t <- s.addRelation(p.numberOfBoundaryPoints, (independentDiagrams(p.numberOfBoundaryPoints) :+ p).zip(relation))
    ) yield t

    whenDenominatorsVanish ++ whenDenominatorsNonzero
  }

  def addRelations(size: Int, relations: Seq[Seq[(PlanarGraph, R)]]): Seq[SpiderData] = {
    relations.foldLeft(Seq(this))({ (spiders, relation) => spiders.flatMap(s => s.addRelation(size, relation)) })
  }

  def addRelation(size: Int, relation: Seq[(PlanarGraph, R)]): Seq[SpiderData] = {
    //    // TODO remove these checks
    //    for (tail <- relation.map(_._1).tails; if tail.nonEmpty) {
    //      for (y <- tail.tail) {
    //        require(tail.head.canonicalFormWithDefect._1 != y.canonicalFormWithDefect._1)
    //      }
    //    }
    addRelations(size - 2, for (
      i <- 0 until size;
      c = spider.stitchAt(relation.toMap, i);
      if !spider.zero_?(c)
    ) yield c.toSeq).flatMap(_._addRelation(size, relation))
  }

  private def _addRelation(size: Int, relation0: Seq[(PlanarGraph, R)]): Seq[SpiderData] = {
    val relation = relation0.map(p => (p._1, normalForm(p._2))).filter(p => !rationalFunctions.zero_?(p._2))
    if (relation.size == 0) {
      Seq(this)
    } else if (relation.size == 1 && size == 0) {
      if (relation.head._1 == PlanarGraph.empty) {
        declarePolynomialZero(relation.head._2.numerator)
      } else {
        _addRelation(0, Seq((PlanarGraph.empty, spider.evaluate(relation.toMap))))
      }
    } else {

      val p = relation.last._1
      val px = relation.last._2

      if (px != rationalFunctions.one) {
        ((for (s <- declarePolynomialZero(px.numerator)) yield {
          require(s.rationalFunctions.zero_?(s.normalForm(px)))
          val newRelation = relation.map(q => (q._1, s.normalForm(q._2))).filter(q => !s.rationalFunctions.zero_?(q._2))
          require(newRelation.size < relation.size)
          s._addRelation(size, newRelation)
        }) ++
          (for ((s, i) <- invertRationalFunction(px)) yield {
            s._addRelation(size, relation.map(q => (q._1, s.rationalFunctions.multiply(q._2, i))))
          })).flatten
      } else {
        val pairings =
          for (i <- 0 until size) yield {
            spider.evaluate(spider.multiply(relation.toMap, spider.rotate(Map(p -> polynomials.one), i), size))
          }

        // is it a reducing relation?
        val nonzeroPositions = relation.dropRight(1).zipWithIndex.collect({ case ((d, x), i) if !rationalFunctions.zero_?(normalForm(x)) => i })
        val reducing = nonzeroPositions.forall({ i => complexity.lt(relation(i)._1, relation.last._1) })
        println(s"reducing: $reducing")

        // There's a lemma here. If b \in span{a1, ..., an}, and {a1, ..., ak} is maximally visibly independent,
        // then the inner product determinant for {a1, ..., ak, b} vanishes.
        (for (
          s <- declareAllPolynomialsZero(pairings.map(_.numerator)) //;
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

            val newReduction = Reduction(p, relation.dropRight(1).map(q => (q._1, s.rationalFunctions.negate(q._2))).toMap)
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
                  case None => rationalFunctions.zero
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

    // TODO verify these inequalities are sane
    val addVisibly = addVisiblyIndependentDiagram(p).filter({ s =>
      s.visiblyIndependentDiagrams(p.numberOfBoundaryPoints).size >= 2 * independentDiagrams(p.numberOfBoundaryPoints).size + 2 - dimensionBounds(p.numberOfBoundaryPoints)
    })
    val addInvisibly = if (visiblyIndependentDiagrams(p.numberOfBoundaryPoints).size >= 2 * independentDiagrams(p.numberOfBoundaryPoints).size + 2 - dimensionBounds(p.numberOfBoundaryPoints)) {
      addInvisiblyIndependentDiagram(p)
    } else {
      Seq.empty
    }

    addVisibly ++ addInvisibly
  }

  def addVisiblyIndependentDiagram(p: PlanarGraph): Seq[SpiderData] = {
    println("adding a visibly independent diagram: " + p)

    val invisibleDiagrams = independentDiagrams(p.numberOfBoundaryPoints).filterNot(visiblyIndependentDiagrams(p.numberOfBoundaryPoints).contains)
    println("there are currently " + invisibleDiagrams.size + " invisible diagrams")
    //    val invisibleSubsets = {
    //      import net.tqft.toolkit.collections.Subsets._
    //      invisibleDiagrams.subsets.map(_.toSeq).toSeq.ensuring(s => s.size == 1 << invisibleDiagrams.size)
    //    }
    // actually we only need to consider subsets of size one
    val invisibleSubsets = invisibleDiagrams.map(Seq(_)) :+ Seq.empty
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

    val newIndependentDiagrams = independentDiagrams.updated(p.numberOfBoundaryPoints, independentDiagrams(p.numberOfBoundaryPoints) :+ p)

    (for ((s, (nonzero, allZero)) <- determinantsWithBiggerDeterminants) yield {
      val newVisiblyIndependentDiagrams = visiblyIndependentDiagrams.updated(p.numberOfBoundaryPoints, visiblyIndependentDiagrams(p.numberOfBoundaryPoints) ++ s :+ p)

      invertRationalFunction(nonzero).map(_._1).toSeq.flatMap(_.declareAllPolynomialsZero(allZero.map(_.numerator))).map(_.copy(independentDiagrams = newIndependentDiagrams, visiblyIndependentDiagrams = newVisiblyIndependentDiagrams))
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
        calculateDeterminant(visiblyIndependentDiagrams(p.numberOfBoundaryPoints) ++ s :+ p)
      }
    }

    val newIndependentDiagrams = independentDiagrams.updated(p.numberOfBoundaryPoints, independentDiagrams(p.numberOfBoundaryPoints) :+ p)

    declareAllPolynomialsZero(determinants.map(_.numerator)).map(_.copy(independentDiagrams = newIndependentDiagrams))
  }

  def normalizePolynomial(p: P) = {
    def bigRationals = implicitly[Field[Fraction[BigInt]]]
    polynomials.leadingMonomial(p) match {
      case Some(lm) => polynomials.scalarMultiply(bigRationals.inverse(p.coefficients(lm)), p)
      case None => polynomials.zero
    }

  }

  //  def liftDenominators(p: R): P = polynomials.multiply(p.numerator, invertPolynomial(p.denominator).get._2)

  def invertRationalFunction(p: R): Option[(SpiderData, R)] = {
    invertPolynomial(p.numerator).map(q => (q._1, rationalFunctions.multiply(q._2, p.denominator)))
  }

  def invertPolynomial(p: P): Option[(SpiderData, R)] = {
    if (polynomials.zero_?(p)) {
      None
    } else if (p.totalDegree.get == 0) {
      Some((this, Fraction(polynomials.one, p)))
    } else {
      import mathematica.Factor._
      println("Factoring something we're inverting: " + p.toMathematicaInputString)
      val factors = p.factor
      println("Factors: ")
      for (f <- factors) println(f._1.toMathematicaInputString + "   ^" + f._2)

      val result = factors.foldLeft[Option[(SpiderData, R)]](Some(this, rationalFunctions.one))({ (s: Option[(SpiderData, R)], q: (P, Int)) =>
        s.flatMap({
          z: (SpiderData, R) =>
            if (q._2 > 0) {
              z._1.invertIrreduciblePolynomial(q._1).map({ w =>
                (w._1, rationalFunctions.multiply(rationalFunctions.power(w._2, q._2), z._2))
              })
            } else {
              Some((z._1, rationalFunctions.multiply(rationalFunctions.power(q._1, -q._2), z._2)))
            }
        })
      })

      if (polynomials.totalDegree(p) > 0) {
        require(result.isEmpty || result.get._1.groebnerBasis.nonEmpty)
      }

      result
    }
  }

  def invertPolynomials(ps: Seq[P]): Option[(SpiderData, Seq[R])] = {
    ps.foldLeft[Option[(SpiderData, Seq[R])]](Some((this, Seq.empty)))({ (o, p) => o.flatMap(q => q._1.invertPolynomial(p).map(r => (r._1, q._2 :+ r._2))) })
  }

  def invertibilityWitness(p: P): P = {
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
  def invertIrreduciblePolynomial(p: P): Option[(SpiderData, R)] = {
    println("inverting " + p.toMathematicaInputString)
    if (polynomials.zero_?(p)) {
      None
    } else {
      val i = invertibilityWitness(p)
      declareIrreduciblePolynomialZero(polynomials.subtract(polynomials.multiply(i, p), polynomials.one)).map(s => (s, Fraction(polynomials.one, p)))
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
      val result = factors.flatMap(f => {
        require(!f.toMathematicaInputString.contains("^(-1)"));
        val result = declareIrreduciblePolynomialZero(f)
        for (s <- result; if !s.polynomials.zero_?(r)) {
          implicit val mf = MathematicaForm.multivariablePolynomialMathematicaForm(implicitly[MathematicaForm[Fraction[BigInt]]], MathematicaForm.StringMathematicaForm)
          println("declarePolynomialZero experienced a problem!")
          println("Groebner basis:")
          for (p <- groebnerBasis) {
            println(p.toMathematicaInputString)
          }
          println("Polynomial:")
          println(r.toMathematicaInputString)
          println("Factor:")
          println(f.toMathematicaInputString)
          println("New Groebner basis:")
          for (p <- s.groebnerBasis) {
            println(p.toMathematicaInputString)
          }
          require(false)
        }
        result
      })
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
          case Reduction(big, small) => Reduction(big, small.map({ p => (p._1, Fraction(newPolynomials.normalForm(p._2.numerator), newPolynomials.normalForm(p._2.denominator))) }))
        })
        val newNonReducingRelations = nonReducingRelations.map(_.map(_.map(p => Fraction(newPolynomials.normalForm(p.numerator), newPolynomials.normalForm(p.denominator)))))
        val result = Some(copy(
          spider = spider.copy(extraReductions = newExtraReductions),
          nonReducingRelations = newNonReducingRelations,
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

