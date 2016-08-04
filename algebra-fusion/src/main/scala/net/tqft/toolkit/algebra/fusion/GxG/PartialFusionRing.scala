package net.tqft.toolkit.algebra.fusion.GxG

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.grouptheory.Orbit

case class SmallGroup(order: Int, index: Int)

case class PartialFusionRingContext(invertibles: SmallGroup, globalDimensionBound: Double) {

  case class GxGOrbit(name: Int, index: Int)
  // X^* = dual._1 X dual._2
  case class SelfDualOrbit(orbit: GxGOrbit, dual: (Int, Int))
  // X^* = dual._1 Y dual._2
  case class DualPairOrbit(orbit1: GxGOrbit, orbit2: GxGOrbit, dual: (Int, Int))
  
  
  case class PartialFusionRing(
      previousDepth: Option[PartialFusionRing],
      selfDualOrbits: IndexedSeq[SelfDualOrbit],
      dualPairOrbits: IndexedSeq[DualPairOrbit]) extends CanonicalGeneration[PartialFusionRing, IndexedSeq[Int]] {

    sealed trait Lower {
      def result: PartialFusionRing
    }
    case object DecreaseDepth extends Lower {
      override def result = previousDepth.get
    }
    case class DeleteSelfDualOrbit(index: Int) extends Lower {
      override def result = ???
    }
    case class DeleteDualPairOrbit(index: Int) extends Lower {
      override def result = ???
    }

    val automorphisms: FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = ???
    val lowerObjects: automorphisms.ActionOnFiniteSet[Lower] = ???
    def ordering: Ordering[Orbit[IndexedSeq[Int], Lower]] = ???
    def upperObjects: automorphisms.ActionOnFiniteSet[Upper] = ???
  }

}

