package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.matrices2.Matrices
import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import net.tqft.toolkit.algebra.graphs.ColouredGraph
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.algebra.graphs.Graph
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import net.tqft.toolkit.algebra.enumeration.CanonicalGenerationWithIsomorphism
import net.tqft.toolkit.Logging

case class PartialFusionRingEnumeration(numberOfSelfDualObjects: Int, numberOfDualPairs: Int, globalDimensionUpperBound: Option[Double] = None) { enumeration =>

  val rank = numberOfSelfDualObjects + 2 * numberOfDualPairs
  def dual(i: Int) = {
    if (i < numberOfSelfDualObjects) {
      i
    } else {
      if ((i - numberOfSelfDualObjects) % 2 == 0) {
        i + 1
      } else {
        i - 1
      }
    }
  }
  // Fusion multiplicities are labelled by a triple (Int, Int, Int), with (x,y,z) representing the multiplicity of z in x \otimes y.
  // We don't use all rank^3 fusion multiplicities, because some must be equal by reciprocity and semisimplicity.
  // synonymousMultiplicities returns the list of multiplicities which must be equal to the given one.
  def synonymousMultiplicities(p: (Int, Int, Int)) = {
    val (a, b, c) = p
    // TODO Dave; please verify these are correct at some point.
    Seq((c, dual(b), a), (dual(a), c, b), (a, b, c), (b, dual(c), dual(a)), (dual(c), a, dual(b)), (dual(b), dual(a), dual(c)))
  }
  // multiplicityNamer takes a triple of Ints, and returns the 'preferred variable' in the reciprocity class.
  // (In particular, the variables we work with are the image of this function on {0,...,rank-1}^3.)
  def multiplicityNamer(a: Int, b: Int, c: Int) = {
    synonymousMultiplicities((a, b, c)).min
  }
  private val dualitySubstitutions = (for (i <- 0 until rank; j <- 0 until rank; k = if (i == dual(j)) 1 else 0) yield (multiplicityNamer(i, j, 0), k))

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

  val root = {
    val associativity = dualitySubstitutions.foldLeft(
      SystemOfQuadratics(Set.empty, AssociativityConstraints(rank, multiplicityNamer _).map(q => QuadraticState(q._1, q._2))))({
        case (system, (s, k)) => system.substitute(s, k, levelOverride = Some(0)).get
      }).factor
    val matrices = IndexedSeq.tabulate(rank, rank, rank)({
      case (i, j, 0) => if (i == dual(j)) 1 else 0
      case (i, 0, j) => if (i == j) 1 else 0
      case (0, i, j) => if (i == j) 1 else 0
      case _ => 1
    })
    PartialFusionRing(
      None,
      Seq.empty,
      Seq.tabulate(rank - 1, rank - 1, rank - 1)({ case (i, j, k) => multiplicityNamer(i + 1, j + 1, k + 1) }).flatten.flatten.toSet,
      Some(associativity),
      Some(matrices))
  }

  private val stringNamer = {
    root.remaining.toSeq.sorted.zip(('A' to 'Z') ++ ('a' to 'z')).toMap
  }

  object PartialFusionRing {
    def apply(shortString: String): PartialFusionRing = {
      import net.tqft.toolkit.Extractors._
      val Seq(objects, Int(level), matrices, Double(dimension)) = shortString.split(" ").toSeq
      require(level < 10)
      require(objects.split(",").map(_.toInt).toSeq == Seq(numberOfSelfDualObjects, numberOfDualPairs))
      (0 to level).foldLeft(root)({
        case (pfr, l) => {
          val lc = l.toString.head
          val entries = (
            for (
              i <- 1 until rank;
              j <- 1 until rank;
              k <- 1 until rank;
              if (matrices((i - 1) * (rank - 1) * (rank - 1) + (j - 1) * (rank - 1) + (k - 1))) == lc
            ) yield multiplicityNamer(i, j, k)).toSet
          entries.foldLeft(pfr)({ (r, z) => r.addEntryIfAssociative(z).get.result }).IncreaseLevel.result
        }
      }).previousLevel.get
    }
  }

  // matrices contains the current fusion multiplicities, with all as-yet unspecified entries set at level+1
  case class PartialFusionRing(
    previousLevel: Option[PartialFusionRing],
    entries: Seq[(Int, Int, Int)],
    remaining: Set[(Int, Int, Int)],
    associativityOption: Option[SystemOfQuadratics[(Int, Int, Int)]],
    matricesOption: Option[IndexedSeq[IndexedSeq[IndexedSeq[Int]]]]) extends CanonicalGenerationWithIsomorphism[PartialFusionRing, IndexedSeq[Int]] { pfr =>

    require(entries.forall(_._3 != 0))

    def findIsomorphismTo(other: enumeration.PartialFusionRing) = {
      Dreadnaut.findIsomorphism(graphPresentation, other.graphPresentation).map(_.take(rank))
    }
    def imageUnderPermutation(g: IndexedSeq[Int]): PartialFusionRing = {
      require(g(0) == 0)
      PartialFusionRing(previousLevel.map(_.imageUnderPermutation(g)), entries.map(v => multiplicityNamer(g(v._1), g(v._2), g(v._3))), remaining.map(v => multiplicityNamer(g(v._1), g(v._2), g(v._3))), None, None)
    }
    def isomorphs = FiniteGroups.symmetricGroup(rank).elements.filter(_(0) == 0).map(g => imageUnderPermutation(g)).iterator

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
                if (level == 0 && (i == 0 || j == 0 || (k == 0 && i == dual(j)))) {
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
      s"PartialFusionRing(level = $level, entries = ${entries.map(stringNamer)}, remaining = ${remaining.map(stringNamer)})     globalDimensionLowerBound = $globalDimensionLowerBound\n" +
        matricesToString //+
      //                  "closedVariablesByNumberOfVariables: " + associativity.closedVariablesByNumberOfVariables.map({ p => stringNamer(p._1) -> p._2 }) + "\n" +
      //                  associativityToString
    }
    def toShortString: String = {
      require(level < 10)
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
      numberOfSelfDualObjects + "," + numberOfDualPairs + " " + level + " " + matrices.tail.map(_.tail.map(_.tail.map(writeEntry).mkString("")).mkString("")).mkString("") + " " + short(globalDimensionLowerBound)
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
      (for (m <- squares) yield FrobeniusPerronEigenvalues.estimate(m.toArray.map(_.toArray))).map(fixDimension).sum
    }

    lazy val globalDimensionLowerBoundAfterIncreasingLevel: Double = {
      val matrixRing = Matrices.ofSize[Int](rank)
      val squares = for (m <- matrices) yield matrixRing.multiply(m, m.transpose)
      (for (m <- squares) yield FrobeniusPerronEigenvalues.estimate(m.toArray.map(_.toArray))).map(fixDimension).sum
    }

    trait Upper {
      val result: PartialFusionRing
      def inverse: result.Lower
    }
    case object IncreaseLevel extends Upper {
      override val result = {
        val newMatrices = IndexedSeq.tabulate(rank, rank, rank)({
          case (i, j, k) => if (i == 0 || j == 0 || (k == 0 && i == dual(j)) || matrices(i)(j)(k) <= level) {
            matrices(i)(j)(k)
          } else {
            level + 2
          }
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
      val colours = IndexedSeq.fill(rank)(-3) ++ IndexedSeq.fill(rank)(-2) ++ IndexedSeq.fill(rank)(-1) ++ matrices.map(_.flatten).flatten :+ level
      unlabelledGraph.colour(colours)
    }
    override lazy val automorphisms: net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = {
      FiniteGroups.symmetricGroup(rank).subgroupGeneratedBy(graphPresentation.automorphismGroup.generators.map(_.take(rank)))
    }

    override lazy val lowerObjects: automorphisms.Action[Lower] = {
      new automorphisms.Action[Lower] {
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

    private def actionOn(variables: Seq[(Int, Int, Int)]): automorphisms.Action[(Int, Int, Int)] = {
      new automorphisms.Action[(Int, Int, Int)] {
        override def elements = variables
        override def act(g: IndexedSeq[Int], v: (Int, Int, Int)) = multiplicityNamer(g(v._1), g(v._2), g(v._3))
      }
    }

//    lazy val targets = actionOn(remaining.toSeq).orbits.map(_.representative).flatMap(addEntryIfAssociative)
    lazy val targets = remaining.flatMap(addEntryIfAssociative)
    lazy val frequentVariables = associativity.mostFrequestVariablesInQuadraticsWithFewestVariables(targets.map(_.m))

    override lazy val upperObjects: automorphisms.Action[Upper] = {
      new automorphisms.Action[Upper] {
        override def elements: Seq[Upper] = {
          (if (remaining.nonEmpty && associativity.quadratics.forall(q => (q.zero_? || q.completeSubstitution.sign == 0) && !q.completeSubstitution.impossibleAtLevel(level + 1)) && (globalDimensionUpperBound.isEmpty || globalDimensionLowerBoundAfterIncreasingLevel < globalDimensionUpperBound.get)) {
            Seq(IncreaseLevel)
          } else {
            Seq.empty
          }) ++ ({

            Logging.info("I think children should be amongst:")
            Logging.info(targets.filter(t => frequentVariables.contains(t.m)))
            targets
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

    override def children = {
      val result = super.children
      for (c <- result) {
        require(c.level == level + 1 || frequentVariables.contains((c.entries.toSet -- entries.toSet).head))
      }
      result
    }

    override lazy val ordering: Ordering[lowerObjects.Orbit] = {
      import net.tqft.toolkit.orderings.Orderings._
      import net.tqft.toolkit.orderings.LexicographicOrdering

      val reverseIntOrdering = implicitly[Ordering[Int]].reverse
      // we prefer deleting a variable which would appear in the most equations with the fewest variables
      // (i.e. we only compare the number of equations with more variables if there are ties with fewer variables)
      implicit val mapOrdering = LexicographicOrdering.mapOrdering[Int, Int](reverseIntOrdering, reverseIntOrdering)

      implicit val invariantOrdering: Ordering[Lower] = Ordering.by[Lower, Int]({
        case DecreaseLevel => 0
        case DeleteEntry(_) => 1
      }).refineByPartialFunction({
        case DeleteEntry(v) => {
          associativity.closedVariablesByNumberOfVariables.get(v) //.getOrElse(Map.empty) // TODO remove this, no longer necessary
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

object PartialFusionRingEnumeration {
  import net.tqft.toolkit.algebra.grouptheory.FiniteGroup
  def fromFiniteGroup[S](G: FiniteGroup[S]): PartialFusionRingEnumeration#PartialFusionRing = {
    val rank = G.size
    val elementsSeq = G.elements.toIndexedSeq
    val numberOfSelfDualObjects = G.elements.count(x => G.inverse(x) == x)
    val enumeration = PartialFusionRingEnumeration(numberOfSelfDualObjects, (rank - numberOfSelfDualObjects) / 2)
    val zeroEntries = (for (i <- 1 until rank; j <- 1 until rank; k <- 1 until rank; if G.multiply(elementsSeq(i), elementsSeq(j)) != elementsSeq(k)) yield enumeration.multiplicityNamer(i, j, k)).distinct
    val oneEntries = (for (i <- 1 until rank; j <- 1 until rank; k = elementsSeq.indexOf(G.multiply(elementsSeq(i), elementsSeq(j))); if k != 0) yield enumeration.multiplicityNamer(i, j, k)).distinct
    val withZeroEntries = zeroEntries.foldLeft(enumeration.root)({
      case (pfr, m) => pfr.addEntryIfAssociative(m).get.result
    })
    if (oneEntries.nonEmpty) {
      oneEntries.foldLeft(withZeroEntries.IncreaseLevel.result)({
        case (pfr, m) => pfr.addEntryIfAssociative(m).get.result
      })
    } else {
      withZeroEntries
    }
  }
}
