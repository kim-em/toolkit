package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.matrices2.Matrices
import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues

case class PartialFusionRingEnumeration(numberOfSelfDualObjects: Int, numberOfDualPairs: Int) {

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
  def multiplicityNamer(a: Int, b: Int, c: Int) = {
    synonymousMultiplicities((a, b, c)).min
  }
  def synonymousMultiplicities(p: (Int, Int, Int)) = {
    val (a, b, c) = p
    Seq((c, dual(b), a), (dual(a), c, b), (a, b, c), (b, dual(c), dual(a)), (dual(c), a, dual(b)), (dual(b), dual(a), dual(c)))
  }
  val dualitySubstitutions = (for (i <- 0 until rank; j <- 0 until rank; k = if (i == dual(j)) 1 else 0) yield (multiplicityNamer(i, j, 0), k))

  case class PartialFusionRing(
    previousLevel: Option[PartialFusionRing],
    entries: Set[(Int, Int, Int)],
    associativity: SystemOfQuadratics[(Int, Int, Int)],
    matrices: IndexedSeq[IndexedSeq[IndexedSeq[Int]]]) extends CanonicalGeneration[PartialFusionRing, IndexedSeq[Int]] { pfr =>

    val level: Int = previousLevel match {
      case Some(p) => p.level + 1
      case None => 0
    }

    def globalDimensionLowerBound: Double = {
      val matrixRing = Matrices.ofSize[Int](rank)
      val squares = for (m <- matrices) yield matrixRing.multiply(m, m.transpose)
      (for(m <- squares) yield FrobeniusPerronEigenvalues.estimate(m.toArray.map(_.toArray))).sum
    }

    trait Upper {
      val result: PartialFusionRing
      def inverse: result.Lower
    }
    case object IncreaseLevel extends Upper {
      override val result = PartialFusionRing(Some(pfr), Set.empty, associativity.resetHistory, ???)
      override def inverse = result.DecreaseLevel
    }
    case class AddEntry(m: (Int, Int, Int), result: PartialFusionRing) extends Upper {
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
        val replayedQuadratics = SystemOfQuadratics(associativity.quadratics.map(qh => qh.copy(current = remainingEntries.foldLeft(qh.original)({ case (q, n) => q.substitute(n, level) }))))
        PartialFusionRing(previousLevel, remainingEntries, replayedQuadratics, ???)
      }
    }

    override val automorphisms: net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = ???

    override val lowerObjects: automorphisms.Action[Lower] = ???
    override val upperObjects: automorphisms.Action[Upper] = ???

    override val ordering: Ordering[lowerObjects.Orbit] = ???

  }
}