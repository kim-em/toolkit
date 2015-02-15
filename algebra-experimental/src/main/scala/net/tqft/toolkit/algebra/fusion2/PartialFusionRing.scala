package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.matrices2.Matrices
import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import net.tqft.toolkit.algebra.graphs.ColouredGraph
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.algebra.graphs.Graph
import net.tqft.toolkit.algebra.graphs.Dreadnaut

case class PartialFusionRingEnumeration(numberOfSelfDualObjects: Int, numberOfDualPairs: Int, globalDimensionBound: Double = 12.0) {

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
        }).map(_.flatten).flatten
    }
    Graph(3 * rank + rank * rank * rank, adjacencies)
  }

  val root = {
    val associativity = dualitySubstitutions.foldLeft(
      SystemOfQuadratics(AssociativityConstraints.apply(rank, multiplicityNamer _)))({
        case (system, (s, k)) => system.substitute(s, k).get
      }).factor
    val matrices = IndexedSeq.tabulate(rank, rank, rank)({
      case (i, j, 0) => if (i == dual(j)) 1 else 0
      case (i, 0, j) => if (i == j) 1 else 0
      case (0, i, j) => if (i == j) 1 else 0
      case _ => 1
    })
    PartialFusionRing(
      None,
      Set.empty,
      Some(associativity),
      Some(matrices))
  }

  // matrices contains the current fusion multiplicities, with all as-yet unspecified entries set at level+1
  case class PartialFusionRing(
    previousLevel: Option[PartialFusionRing],
    entries: Set[(Int, Int, Int)],
    associativityOption: Option[SystemOfQuadratics[(Int, Int, Int)]],
    matricesOption: Option[IndexedSeq[IndexedSeq[IndexedSeq[Int]]]]) extends CanonicalGeneration[PartialFusionRing, IndexedSeq[Int]] { pfr =>

    def associativity = associativityOption.get
    def matrices = matricesOption.get

    override def equals(other: Any) = {
      other match {
        case other: PartialFusionRing => previousLevel == other.previousLevel && entries == other.entries
        case _ => false
      }
    }
    override def hashCode() = {
      (previousLevel, entries).hashCode
    }

    val level: Int = previousLevel match {
      case Some(p) => p.level + 1
      case None => 0
    }

    def globalDimensionLowerBoundAfterIncreasingLevel: Double = {
      val matrixRing = Matrices.ofSize[Int](rank)
      val squares = for (m <- matrices) yield matrixRing.multiply(m, m.transpose)
      (for (m <- squares) yield FrobeniusPerronEigenvalues.estimate(m.toArray.map(_.toArray))).sum
    }

    trait Upper {
      val result: PartialFusionRing
      def inverse: result.Lower
    }
    case object IncreaseLevel extends Upper {
      override val result = {
        val newMatrices = IndexedSeq.tabulate(rank, rank, rank)({
          case (i, j, k) => if (i == 0 || j == 0 || i == dual(j) || matrices(i)(j)(k) <= level) {
            matrices(i)(j)(k)
          } else {
            level + 2
          }
        })
        PartialFusionRing(Some(pfr), Set.empty, associativityOption, Some(newMatrices))
      }
      override def inverse = result.DecreaseLevel
    }
    case class AddEntry(m: (Int, Int, Int), quadratics: Option[SystemOfQuadratics[(Int, Int, Int)]]) extends Upper {
      override val result: PartialFusionRing = PartialFusionRing(
        previousLevel,
        entries + m,
        quadratics,
        Some(synonymousMultiplicities(m).foldLeft(matrices)({
          case (ms, v) => ms.updated(v._1, ms(v._1).updated(v._2, ms(v._1)(v._2).updated(v._3, level)))
        })))
      override def inverse = result.DeleteEntry(m)

      override def equals(other: Any) = {
        other match {
          case AddEntry(m2, _) => m == m2
          case _ => false
        }
      }
      override def hashCode = (pfr, m).hashCode
    }

    trait Lower {
      def result: PartialFusionRing
    }
    case object DecreaseLevel extends Lower {
      override def result = previousLevel.get
    }
    case class DeleteEntry(m: (Int, Int, Int)) extends Lower {
      override def result = {
        val remainingEntries = entries - m
        PartialFusionRing(previousLevel, remainingEntries, None, ???)
      }
    }

    private lazy val graph = {
      val colours = IndexedSeq.fill(rank)(-3) ++ IndexedSeq.fill(rank)(-2) ++ IndexedSeq.fill(rank)(-1) ++ matrices.map(_.flatten).flatten
      unlabelledGraph.colour(colours)
    }
    override lazy val automorphisms: net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = {
      FiniteGroups.symmetricGroup(rank).subgroupGeneratedBy(graph.automorphismGroup.generators.map(_.take(rank)))
    }

    override lazy val lowerObjects: automorphisms.Action[Lower] = {
      new automorphisms.Action[Lower] {
        override def elements: Seq[Lower] = {
          (if (entries.isEmpty) {
            Seq(DecreaseLevel)
          } else {
            Seq.empty
          }) ++ (entries.map(DeleteEntry))
        }
        override def act(g: IndexedSeq[Int], l: Lower): Lower = {
          l match {
            case DecreaseLevel => DecreaseLevel
            case DeleteEntry(m) => DeleteEntry((g(m._1), g(m._2), g(m._3)))
          }
        }
      }
    }
    override lazy val upperObjects: automorphisms.Action[Upper] = {
      new automorphisms.Action[Upper] {
        override def elements: Seq[Upper] = {
          (if (globalDimensionLowerBoundAfterIncreasingLevel < globalDimensionBound) {
            Seq(IncreaseLevel)
          } else {
            Seq.empty
          }) ++ (
            associativity.mostFrequentVariablesInMinimalEquations.flatMap({
              case v => associativity.substitute(v, level).map({ quadratics =>
                AddEntry(v, Some(quadratics))
              })
            }))
        }
        override def act(g: IndexedSeq[Int], u: Upper): Upper = {
          u match {
            case IncreaseLevel => IncreaseLevel
            case AddEntry(m, quadratics) => AddEntry((g(m._1), g(m._2), g(m._3)), None)
          }
        }
      }
    }

    override lazy val ordering: Ordering[lowerObjects.Orbit] = {
      import net.tqft.toolkit.orderings.Orderings._

      implicit val invariantOrdering: Ordering[Lower] = Ordering.by[Lower, Int]({
        case DecreaseLevel => 0
        case DeleteEntry(_) => 1
      }).refineByPartialFunction({
        case DeleteEntry(v) => associativity.closedVariableTalliesInMinimalEquations(v)
      }).refineByPartialFunction({
        case DeleteEntry((i, j, k)) => Dreadnaut.canonicalizeColouredGraph(graph.additionalMarking(Seq(3 * rank + i * rank * rank + j * rank + k)))
      })

      Ordering.by({ o: lowerObjects.Orbit =>
        o.representative
      })
    }

  }
}