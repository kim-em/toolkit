package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.matrices2.Matrices
import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.algebra.graphs._
import net.tqft.toolkit.algebra.enumeration._
import net.tqft.toolkit.algebra.grouptheory.FiniteGroup
import net.tqft.toolkit.arithmetic.MinMax._
import net.tqft.toolkit.collections.DeleteOne._
import net.tqft.toolkit.orderings.LexicographicOrdering
import net.tqft.toolkit.orderings.Orderings.RefineByable

object PartialFusionRingWithInvertibles {
  def apply(shortString: String) = {
    import net.tqft.toolkit.Extractors._
    shortString.split(" ").toSeq match {
      case Seq(Int(_), OrbitStructure(os), dualDataString, Int(_), _, _) => {
        val dualData = dualDataString.split(",").map(_.toInt).toIndexedSeq
        PartialFusionRingWithInvertiblesEnumeration(os, dualData, None).PartialFusionRing(shortString)
      }
    }
  }
}

case class PartialFusionRingWithInvertiblesEnumeration(orbitStructure: OrbitStructure, dualData: IndexedSeq[Int], globalDimensionUpperBound: Option[Double] = None) { enumeration =>
  
  val rank = orbitStructure.groupOrder + orbitStructure.orbitSizes.sum

  val minimumDimensions = orbitStructure.objectTypes.map(_.dimension)
  val maximumDimensions = orbitStructure.objectTypes.map({
    case obj: SmallFusionObjectWithDefiniteDimension => Some(obj.dimension)
    case _ => None
  })

  // Fusion multiplicities are labelled by a triple (Int, Int, Int), with (x,y,z) representing the multiplicity of z in x \otimes y.
  // We don't use all rank^3 fusion multiplicities, because some must be equal by reciprocity and semisimplicity.
  // synonymousMultiplicities returns the list of multiplicities which must be equal to the given one.
  def synonymousMultiplicities(p: (Int, Int, Int)) = {
    val (a, b, c) = p
    Seq((c, dualData(b), a), (dualData(a), c, b), (a, b, c), (b, dualData(c), dualData(a)), (dualData(c), a, dualData(b)), (dualData(b), dualData(a), dualData(c)))
  }
  // multiplicityNamer takes a triple of Ints, and returns the 'preferred variable' in the reciprocity class.
  // (In particular, the variables we work with are the image of this function on {0,...,rank-1}^3.)
  def multiplicityNamer(a: Int, b: Int, c: Int) = {
    synonymousMultiplicities((a, b, c)).min
  }

  private val unlabelledGraph = {
    val adjacencies = {
      IndexedSeq.tabulate(rank)(i => Seq(i + rank)) ++
        IndexedSeq.tabulate(rank)(i => Seq(i + 2 * rank)) ++
        IndexedSeq.tabulate(rank)(i => Seq(i)) ++
        IndexedSeq.tabulate(rank, rank, rank)({
          case (i, j, k) => Seq(i, j + rank, k + 2 * rank)
        }).map(_.flatten).flatten :+
        IndexedSeq.empty // an isolated vertex, to mark with the level
    }
    Graph(3 * rank + rank * rank * rank + 1, adjacencies)
  }

  def orbitIndexPairToIndex(orbit: Int, indexWithinOrbit: Int): Int = {
    orbitStructure.groupOrder + orbitStructure.orbitSizes.take(orbit).sum + indexWithinOrbit
  }

  val rootSubstitutions = {
    def dualitySubstitutions = (for (i <- 0 until rank; j <- 0 until rank; k = if (i == dualData(j)) 1 else 0) yield (multiplicityNamer(i, j, 0), k))
    def groupStructureSubstitutions = {
      for (
        i <- 0 until orbitStructure.groupOrder;
        j <- 0 until orbitStructure.groupOrder;
        k <- 0 until rank;
        m = if (k >= orbitStructure.groupOrder) 0 else {
          if (orbitStructure.groupMultiplication(i, j) == k) 1 else 0
        }
      ) yield (multiplicityNamer(i, j, k), m)
    }

    def groupActionSubstitutions = {
      for (
        i <- 0 until orbitStructure.groupOrder;
        g = orbitStructure.elements(i);
        (((action, _), xSize), xOrbit) <- orbitStructure.actionObjectPairs.zip(orbitStructure.orbitSizes).zipWithIndex;
        (((_, _), ySize), yOrbit) <- orbitStructure.actionObjectPairs.zip(orbitStructure.orbitSizes).zipWithIndex;
        xIndex <- 0 until xSize;
        yIndex <- 0 until ySize
      ) yield {
        val m = if (xOrbit == yOrbit) {
          orbitStructure.group.elements
          if (orbitStructure.actions(action).act(g, xIndex) == yIndex) {
            1
          } else {
            0
          }
        } else {
          0
        }
        multiplicityNamer(i, orbitIndexPairToIndex(xOrbit, xIndex), orbitIndexPairToIndex(yOrbit, yIndex)) -> m
      }
    }
    def XXdualSubstitutions = {
      for (
        (((_, objectType), xSize), xOrbit) <- orbitStructure.actionObjectPairs.zip(orbitStructure.orbitSizes).zipWithIndex;
        x <- 0 until xSize;
        if objectType.XXdual.isInstanceOf[Exactly];
        Exactly(m) = objectType.XXdual;
        xd = dualData(orbitIndexPairToIndex(xOrbit, x));
        (((_, objectType), ySize), yOrbit) <- orbitStructure.actionObjectPairs.zip(orbitStructure.orbitSizes).zipWithIndex;
        if (!m.contains(objectType));
        y <- 0 until ySize
      ) yield {
        multiplicityNamer(orbitIndexPairToIndex(xOrbit, x), xd, orbitIndexPairToIndex(yOrbit, y)) -> 0
      }
    }

    val result = (dualitySubstitutions ++ groupStructureSubstitutions ++ groupActionSubstitutions ++ XXdualSubstitutions).toMap
//    println(enumeration)
//    println(result)
    result
  }

  val rootOption = {

    val XXdualEquations: Seq[QuadraticState[(Int, Int, Int)]] = {
      // some linear equations, expressing the known summands of XXdual
      for (
        (((_, objectType), xSize), xOrbit) <- orbitStructure.actionObjectPairs.zip(orbitStructure.orbitSizes).zipWithIndex;
        if objectType.XXdual.isInstanceOf[Exactly] || objectType.XXdual.isInstanceOf[AtLeast];
        PartialKnowledge(map) = objectType.XXdual;
        (objectType, multiplicity) <- map;
        xi <- 0 until xSize;
        x = orbitIndexPairToIndex(xOrbit, xi);
        xd = dualData(x)
      ) yield {
        val terms = (objectType match {
          case TrivialObject => {
            Seq((x, xd, 0) -> 1)
          }
          case NonTrivialObject => {
            for (k <- 1 until orbitStructure.groupOrder) yield {
              (x, xd, k) -> 1
            }
          }
          case InvolutionObject => {
            for (
              k <- 1 until orbitStructure.groupOrder;
              if orbitStructure.groupMultiplication(k, k) == 0
            ) yield {
              (x, xd, k) -> 1
            }
          }
          case _ => {
            for (
              (obj, k) <- orbitStructure.objectTypes.zipWithIndex;
              if obj == objectType
            ) yield {
              (orbitIndexPairToIndex(xOrbit, x), xd, k) -> 1
            }
          }
        }).toMap
        val linearTerm: LinearTerm[(Int, Int, Int)] = LinearTerm(-multiplicity, terms)
        QuadraticState(s"XXdual equation for $x, $objectType", Quadratic(linearTerm, Seq.empty))
      }
    }

    val initialEquations = SystemOfQuadratics(Set.empty, AssociativityConstraints(rank, multiplicityNamer _).map(q => QuadraticState(q._1.toString, q._2)) ++ XXdualEquations)
    val associativityOption = rootSubstitutions.foldLeft[Option[SystemOfQuadratics[(Int, Int, Int)]]](Some(initialEquations))({
      case (system, (s, k)) => system.flatMap(_.substitute(s, k, levelOverride = Some(0)))
    }).map(_.factor)
    val matrices = IndexedSeq.tabulate(rank, rank, rank)({
      case (i, j, k) => rootSubstitutions.get(multiplicityNamer(i, j, k)) match {
        case Some(m) => m
        case None => 1 // unknown entries
      }
    })
    associativityOption.map(associativity =>
      PartialFusionRing(
        None,
        Seq.empty,
        Seq.tabulate(rank, rank, rank)({ case (i, j, k) => multiplicityNamer(i, j, k) }).flatten.flatten.toSet -- rootSubstitutions.keySet,
        Some(associativity),
        Some(matrices)))
  }
  def root = rootOption.get

  private lazy val stringNamer = {
    root.remaining.toSeq.sorted.zip(('A' to 'Z') ++ ('a' to 'z')).toMap
  }

  object PartialFusionRing {
    def apply(shortString: String): PartialFusionRing = {
      import net.tqft.toolkit.Extractors._
      shortString.split(" ").toSeq match {
        case Seq(
          Int(rank),
          OrbitStructure(os),
          dualDataString,
          Int(level),
          matricesString,
          Double(globalDimensionLowerBound))  => {
            
            require(rank == enumeration.rank)
            require(os == orbitStructure)
            
          val dualData = dualDataString.split(",").toIndexedSeq.map(_.toInt)
          require(dualData == enumeration.dualData)

          val matrixEntries = if (!matricesString.contains(",")) {
            require(level < 10)
            matricesString.toCharArray().map(_.toString)
          } else {
            matricesString.split(",")
          }
          require(matrixEntries.length == (rank - 1) * (rank - 1) * (rank - 1))
          (0 to level).foldLeft(root)({
            case (pfr, l) => {
              val lc = l.toString
              val entries = (
                for (
                  i <- 1 until rank;
                  j <- 1 until rank;
                  k <- 1 until rank;
                  if (matrixEntries((i - 1) * (rank - 1) * (rank - 1) + (j - 1) * (rank - 1) + (k - 1))) == lc
                ) yield multiplicityNamer(i, j, k)).toSet
              entries.foldLeft(pfr)({ (r, z) => r.addEntryIfAssociative(z).get.result }).IncreaseLevel.result
            }
          }).previousLevel.get

        }
      }
    }
  }

  // matrices contains the current fusion multiplicities, with all as-yet unspecified entries set at level+1
  case class PartialFusionRing(
    previousLevel: Option[PartialFusionRing],
    entries: Seq[(Int, Int, Int)],
    remaining: Set[(Int, Int, Int)],
    associativityOption: Option[SystemOfQuadratics[(Int, Int, Int)]],
    matricesOption: Option[IndexedSeq[IndexedSeq[IndexedSeq[Int]]]]) extends CanonicalGenerationWithIsomorphism[PartialFusionRing, IndexedSeq[Int]] { pfr =>

    def findIsomorphismTo(other: enumeration.PartialFusionRing) = {
      Dreadnaut.findIsomorphism(graphPresentation, other.graphPresentation).map(_.take(rank))
    }
    def imageUnderPermutation(g: IndexedSeq[Int]): PartialFusionRing = {
      ???
      //      require(g(0) == 0)
      //      PartialFusionRing(previousLevel.map(_.imageUnderPermutation(g)), entries.map(v => multiplicityNamer(g(v._1), g(v._2), g(v._3))), remaining.map(v => multiplicityNamer(g(v._1), g(v._2), g(v._3))), None, None)
    }
    def isomorphs = ??? // FiniteGroups.symmetricGroup(rank).elements.filter(_(0) == 0).map(g => imageUnderPermutation(g)).iterator

    def extendsTo(other: PartialFusionRing): Boolean = {
      if (other.level < level) {
        false
      } else if (other.level > level) {
        extendsTo(other.previousLevel.get)
      } else {
        previousLevel == other.previousLevel && entries.forall(other.entries.contains)
      }
    }

    lazy val associativity: SystemOfQuadratics[(Int, Int, Int)] = associativityOption match {
      case Some(a) => a
      case None => {
        //        Logging.warn("Reconstructing associativity for a lower object.")
        entries.foldLeft(previousLevel.map(_.IncreaseLevel.result).getOrElse(root))({ case (pfr, entry) => pfr.addEntryIfAssociative(entry).get.result }).associativityOption.get
      }
    }
    def associativityToString = associativity.mapVariables(stringNamer).toString
    def associativityToMathematicaString = associativity.mapVariables(stringNamer).quadratics.map(_.completeSubstitution).mkString("{\n  ", ",\n  ", "\n}")
    lazy val matrices: IndexedSeq[IndexedSeq[IndexedSeq[Int]]] = matricesOption match {
      case Some(m) => m
      case None => {
        //        Logging.warn("Reconstructing matrices for a lower object.")
        entries.foldLeft(previousLevel.map(_.IncreaseLevel.result).getOrElse(root))({ case (pfr, entry) => pfr.addEntryIfAssociative(entry).get.result }).matricesOption.get
      }
    }
    def matricesToString = {
      val head = Seq.fill(rank)(Seq.fill(2 * rank + 1)("-").mkString).mkString("+", "+", "+\n")
      val m = matrices
      (for (j <- 0 until rank) yield {
        (for (i <- 0 until rank) yield {
          (for (k <- 0 until rank) yield {
            m(i)(j)(k) match {
              case e if e == level + 1 => {
                if (level == 0 && (i == 0 || j == 0 || (k == 0 && i == dualData(j)))) {
                  "1"
                } else {
                  stringNamer(multiplicityNamer(i, j, k))
                }
              }
              case e => e.toString
            }
          }).mkString(" ")
        }).mkString("| ", " | ", " |")
      }).mkString(head, "\n", "\n" + head)
    }

    override def toString = {
      s"PartialFusionRing(level = $level, entries = ${entries.map(stringNamer)}, remaining = ${remaining.map(stringNamer)})     globalDimensionLowerBound = $globalDimensionLowerBound\n" // +
      //              matricesToString +
      //              "closedVariablesByNumberOfVariables: " + associativity.closedVariablesByNumberOfVariables.map({ p => stringNamer(p._1) -> p._2 }) + "\n" +
      //              "quadraticsWithFewestVariables: \n" + associativity.mapVariables(stringNamer).quadraticsWithFewestVariables.mkString(" ", "\n ", "") +
      //                                    associativityToString
    }
    def toShortString: String = {
      def short(d: Double) = {
        require(d != Double.NaN)
        val s = d.toString
        s.take(s.indexOf(".") + 3)
      }
      def writeEntry(n: Int) = {
        if (n == level + 1) {
          "_"
        } else {
          n.toString
        }
      }
      val separator = if (level >= 10) "," else ""

      
        rank + " " +
        orbitStructure.toShortString + " " +
        dualData.mkString(",") + " " +
        level + " " +
        matrices.tail.map(_.tail.map(_.tail.map(writeEntry).mkString(separator)).mkString(separator)).mkString(separator) + " " +
        short(globalDimensionLowerBound)
    }

    override def equals(other: Any) = {
      other match {
        case other: PartialFusionRing => previousLevel == other.previousLevel && entries.toSet == other.entries.toSet
        case _ => false
      }
    }
    override def hashCode() = {
      (previousLevel, entries.toSet).hashCode
    }

    val level: Int = previousLevel match {
      case Some(p) => p.level + 1
      case None => 0
    }
    def steps: Int = previousLevel match {
      case Some(p) => p.steps + 1 + entries.size
      case None => entries.size
    }

    private def fixDimension(d: Double) = {
      if (d.toString == "NaN" || d < 1.0) {
        1.0
      } else {
        d
      }
    }

    lazy val globalDimensionLowerBound: Double = {
      val matrixRing = Matrices.ofSize[Int](rank)
      def r(m: IndexedSeq[IndexedSeq[Int]]) = for (row <- m) yield for (x <- row) yield if (x > level) level else x
      val squares = for (m <- matrices; m0 = r(m)) yield matrixRing.multiply(m0, m0.transpose)
      val squaredDimensions = (for (m <- squares) yield FrobeniusPerronEigenvalues.estimate(m.toArray.map(_.toArray))).map(fixDimension)
      squaredDimensions.zip(minimumDimensions.map(x => x * x)).map(p => math.max(p._1, p._2)).sum
    }

    lazy val squaredDimensionsAfterIncreasingLevel: Seq[Double] = {
      val matrixRing = Matrices.ofSize[Int](rank)
      val squares = for (m <- matrices) yield matrixRing.multiply(m, m.transpose)
      (for (m <- squares) yield FrobeniusPerronEigenvalues.estimate(m.toArray.map(_.toArray))).map(fixDimension)
    }

    lazy val globalDimensionLowerBoundAfterIncreasingLevel: Double = {
      squaredDimensionsAfterIncreasingLevel.zip(minimumDimensions.map(x => x * x)).map(p => math.max(p._1, p._2)).sum
    }

    trait Upper {
      val result: PartialFusionRing
      def inverse: result.Lower
    }
    case object IncreaseLevel extends Upper {
      override val result = {
        val newMatrices = IndexedSeq.tabulate(rank, rank, rank)({
          case (i, j, k) if rootSubstitutions.contains(multiplicityNamer(i, j, k)) || matrices(i)(j)(k) <= level =>
            matrices(i)(j)(k)
          case _ => level + 2
        })

        PartialFusionRing(Some(pfr), Seq.empty, remaining, Some(associativity.factor), Some(newMatrices))
      }
      override def inverse = result.DecreaseLevel
    }
    case class AddEntry(m: (Int, Int, Int), quadratics: Option[SystemOfQuadratics[(Int, Int, Int)]]) extends Upper {
      override val result: PartialFusionRing = PartialFusionRing(
        previousLevel,
        entries :+ m,
        remaining - m,
        quadratics,
        Some(synonymousMultiplicities(m).foldLeft(matrices)({
          case (ms, v) => ms.updated(v._1, ms(v._1).updated(v._2, ms(v._1)(v._2).updated(v._3, level)))
        })))
      override def inverse = result.DeleteEntry(m)

      override def toString = s"AddEntry($m, ...)"
      override def equals(other: Any) = {
        other match {
          case AddEntry(m2, _) => m == m2
          case _ => false
        }
      }
      override def hashCode = (pfr, m).hashCode
    }
    def addEntryIfAssociative(m: (Int, Int, Int)): Option[AddEntry] = {
      val substitutions = associativity.substitute(m, level)
      substitutions.map({ quadratics =>
        AddEntry(m, Some(quadratics))
      })
    }

    trait Lower {
      def result: PartialFusionRing
    }
    case object DecreaseLevel extends Lower {
      override def result = previousLevel.get
    }
    case class DeleteEntry(m: (Int, Int, Int)) extends Lower {
      override def result = {
        import net.tqft.toolkit.collections.DeleteOne._
        PartialFusionRing(previousLevel, entries.deleteOne(m), remaining + m, None, None)
      }
    }

    lazy val graphPresentation = {
      val matrixColours = IndexedSeq.fill(rank)(-3) ++ IndexedSeq.fill(rank)(-2) ++ IndexedSeq.fill(rank)(-1) ++ matrices.map(_.flatten).flatten :+ level
      val orbitColours = IndexedSeq.fill(3)(orbitStructure.objectTypes.map(Some(_))).flatten ++ IndexedSeq.fill(rank*rank*rank+1)(None) 
      unlabelledGraph.colour(matrixColours).combineColours(orbitColours)
    }
    override lazy val automorphisms: net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = {
      FiniteGroups.symmetricGroup(rank).subgroupGeneratedBy(graphPresentation.automorphismGroup.generators.map(_.take(rank)))
    }

    override lazy val lowerObjects: automorphisms.ActionOnFiniteSet[Lower] = {
      new automorphisms.ActionOnFiniteSet[Lower] {
        override def elements: Seq[Lower] = {
          (if (entries.isEmpty && previousLevel.nonEmpty) {
            Seq(DecreaseLevel)
          } else {
            Seq.empty
          }) ++ (entries.map(DeleteEntry))
        }
        override def act(g: IndexedSeq[Int], l: Lower): Lower = {
          l match {
            case DecreaseLevel => DecreaseLevel
            case DeleteEntry(m) => DeleteEntry(multiplicityNamer(g(m._1), g(m._2), g(m._3)))
          }
        }
      }
    }

    private def actionOn(variables: Seq[(Int, Int, Int)]): automorphisms.ActionOnFiniteSet[(Int, Int, Int)] = {
      new automorphisms.ActionOnFiniteSet[(Int, Int, Int)] {
        override def elements = variables
        override def act(g: IndexedSeq[Int], v: (Int, Int, Int)) = multiplicityNamer(g(v._1), g(v._2), g(v._3))
      }
    }

    override lazy val upperObjects: automorphisms.ActionOnFiniteSet[Upper] = {
      def individualDimensionsNotTooBig = {
        squaredDimensionsAfterIncreasingLevel.zip(maximumDimensions).forall({
          case (_, None) => true
          case (squared, Some(max)) => squared <= max * max
        })
      }

      new automorphisms.ActionOnFiniteSet[Upper] {
        override def elements: Seq[Upper] = {
          (if (remaining.nonEmpty &&
            associativity.quadratics.forall(q => (q.zero_? || q.completeSubstitution.sign == 0) &&
              !q.completeSubstitution.impossibleAtLevel(level + 1)) &&
            (globalDimensionUpperBound.isEmpty || globalDimensionLowerBoundAfterIncreasingLevel < globalDimensionUpperBound.get) &&
            individualDimensionsNotTooBig) {
            Seq(IncreaseLevel)
          } else {
            Seq.empty
          }) ++ ({

            val targets = actionOn(remaining.toSeq).allOrbits.map(_.representative).flatMap(addEntryIfAssociative)

            // You can look at what's already been closed, and see that you only need to bother deleting things that are even better.

            // If a variable s has already been closed, and it would open an equation with k variables, we only need to look in variables that appear
            // in equations with at most k variables. And indeed we only need to look for variables that appear more often in equations with exactly k variables.
            val variablesInSmallEquations = {
              if (associativity.quadratics.forall(_.zero_?)) {
                remaining
              } else {
                import net.tqft.toolkit.arithmetic.MinMax._
                // TODO we could do even better; variables that only appear in equations with exactly min variables, we only need to take those that appear often.
                associativity.closedVariablesByNumberOfVariables.values.flatMap(_.keys).minOption match {
                  case Some(min) => (for (q <- associativity.quadratics; if q.variables.size <= min; v <- q.variables) yield v).toSet
                  case None => remaining
                }
              }
            }

            targets.filter(t => variablesInSmallEquations.contains(t.m))
          })
        }
        override def act(g: IndexedSeq[Int], u: Upper): Upper = {
          u match {
            case IncreaseLevel => IncreaseLevel
            case AddEntry(m, quadratics) => AddEntry(multiplicityNamer(g(m._1), g(m._2), g(m._3)), None)
          }
        }
      }
    }

    override lazy val ordering: Ordering[lowerObjects.Orbit] = {
      import net.tqft.toolkit.orderings.Orderings._
      import net.tqft.toolkit.orderings.LexicographicOrdering

      val reverseIntOrdering = implicitly[Ordering[Int]].reverse
      val reverseOptionOrdering = implicitly[Ordering[Option[Int]]].reverse
      // we prefer deleting a variable which would appear in the most equations with the fewest variables
      // (i.e. we only compare the number of equations with more variables if there are ties with fewer variables)
      implicit val mapOrdering = LexicographicOrdering.mapOrdering[Int, Int](reverseIntOrdering, reverseOptionOrdering)

      implicit val invariantOrdering: Ordering[Lower] = Ordering.by[Lower, Int]({
        case DecreaseLevel => 0
        case DeleteEntry(_) => 1
      }).refineByPartialFunction({
        case DeleteEntry(v) => {
          associativity.closedVariablesByNumberOfVariables(v)
        }
      }).refineByPartialFunction({
        case DeleteEntry((i, j, k)) => Dreadnaut.canonicalizeColouredGraph(graphPresentation.additionalMarking(Seq(3 * rank + i * rank * rank + j * rank + k)))
      })

      Ordering.by({ o: lowerObjects.Orbit =>
        o.representative
      })
    }

  }
}

