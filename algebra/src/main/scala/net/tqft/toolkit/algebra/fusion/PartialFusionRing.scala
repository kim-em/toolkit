package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.matrices.Matrix

case class PartialFusionRing(depth: Int, ring: FusionRing[Int], globalDimensionLimit: Double) extends CanonicalGeneration[PartialFusionRing, IndexedSeq[Int]] { pfr =>

  override lazy val hashCode = super.hashCode

  val generator = {
    if (ring.multiply(ring.basis(1), ring.basis(1)).head == 1) {
      ring.basis(1)
    } else {
      ring.add(ring.basis(1), ring.basis(2))
    }
  }
  val depths = ring.depthWithRespectTo(generator)

  lazy val automorphisms = ring.automorphisms(depths)
  val ordering: Ordering[Lower] = ???

  sealed trait Lower {
    def result: PartialFusionRing
  }
  case object ReduceDepth extends Lower {
    override def result = copy(depth = depth - 1)
  }
  case class DeleteSelfDualObject(k: Int) extends Lower {
    override def result = {
      import net.tqft.toolkit.collections.Deleted._
      val newRing = FusionRing(ring.structureCoefficients.deleted(k).map(_.dropRows(Seq(k)).dropColumns(Seq(k))))
      pfr.copy(ring = newRing)
    }

  }
  case class DeleteDualPairOfObjects(k1: Int) extends Lower {
    val k2 = k1 + 1
    require(ring.duality(k1) == k2)
    override def result = {
      import net.tqft.toolkit.collections.Deleted._
      val newRing = FusionRing(ring.structureCoefficients.deleted(Seq(k1, k2)).map(_.dropRows(Seq(k1, k2)).dropColumns(Seq(k1, k2))))
      pfr.copy(ring = newRing)
    }
  }

  def lowerObjects = new automorphisms.Action[Lower] {
    override def elements = {
      if (depth == depths.max) {
        depths.zipWithIndex.collect({
          case (d, i) if d == depth && ring.duality(i) == i => DeleteSelfDualObject(i)
          case (d, i) if d == depth && ring.duality(i) > i => DeleteDualPairOfObjects(i)
        }).toSet
      } else {
        Set(ReduceDepth)
      }
    }
    override def act(a: IndexedSeq[Int], b: Lower): Lower = {
      val ai = {
        import net.tqft.toolkit.permutations.Permutations._
        a.inverse
      }
      b match {
        case ReduceDepth => ReduceDepth
        case DeleteSelfDualObject(k) => DeleteSelfDualObject(ai(k))
        case DeleteDualPairOfObjects(k1) => DeleteDualPairOfObjects(ai(k1))
      }
    }
  }

  sealed trait Upper {
    val result: PartialFusionRing
    def inverse: result.Lower
  }
  case object IncreaseDepth extends Upper {
    override lazy val result = pfr.copy(depth = depth + 1)
    override def inverse = result.ReduceDepth
  }
  case class AddSelfDualObject(newRing: FusionRing[Int]) extends Upper {
    override lazy val result = pfr.copy(ring = newRing)
    override def inverse = result.DeleteSelfDualObject(ring.rank)
  }
  case class AddDualPairOfObjects(newRing: FusionRing[Int]) extends Upper {
    override lazy val result = pfr.copy(ring = newRing)
    override def inverse = result.DeleteDualPairOfObjects(ring.rank)
  }

  def upperObjects = new automorphisms.Action[Upper] {
    override lazy val elements: Set[Upper] = {
      (if (depth == depths.max && ring.partialAssociativityConstraints(depth + 1, depths).forall(p => p._1 == p._2)) {
        Set(IncreaseDepth)
      } else {
        Set.empty
      }) ++
        FusionRings.withAnotherSelfDualObject(ring, depth, depths, globalDimensionLimit).map(AddSelfDualObject) ++
        /* FIXME dual pairs */ Set.empty
    }
    override def act(a: IndexedSeq[Int], b: Upper): Upper = {
      b match {
        case IncreaseDepth => IncreaseDepth
        case AddSelfDualObject(newRing) => AddSelfDualObject(newRing.relabel(a :+ ring.rank))
        case AddDualPairOfObjects(newRing) => ???
      }
    }
  }
}