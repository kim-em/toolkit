package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.matrices2.Matrices
import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues

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
  val dualitySubstitutions = (for (i <- 0 until rank; j <- 0 until rank; k = if (i == dual(j)) 1 else 0) yield (multiplicityNamer(i, j, 0), k))

  def root = {
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
    associativity: Option[SystemOfQuadratics[(Int, Int, Int)]],
    matrices: Option[IndexedSeq[IndexedSeq[IndexedSeq[Int]]]]) extends CanonicalGeneration[PartialFusionRing, IndexedSeq[Int]] { pfr =>

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
      val squares = for (m <- matrices.get) yield matrixRing.multiply(m, m.transpose)
      (for (m <- squares) yield FrobeniusPerronEigenvalues.estimate(m.toArray.map(_.toArray))).sum
    }

    trait Upper {
      val result: PartialFusionRing
      def inverse: result.Lower
    }
    case object IncreaseLevel extends Upper {
      override val result = PartialFusionRing(Some(pfr), Set.empty, associativity, ???)
      override def inverse = result.DecreaseLevel
    }
    case class AddEntry(m: (Int, Int, Int), associativity: SystemOfQuadratics[(Int, Int, Int)]) extends Upper {
      override val result: PartialFusionRing = ???
      override def inverse = result.DeleteEntry(m)
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

    override lazy val automorphisms: net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = ???

    override lazy val lowerObjects: automorphisms.Action[Lower] = ???
    override lazy val upperObjects: automorphisms.Action[Upper] = {
      new automorphisms.Action[Upper] {
        override def elements: Seq[Upper] = ???
        override def act(g: IndexedSeq[Int], u: Upper): Upper = ???
      }
    }

    override lazy val ordering: Ordering[lowerObjects.Orbit] = ???

  }
}