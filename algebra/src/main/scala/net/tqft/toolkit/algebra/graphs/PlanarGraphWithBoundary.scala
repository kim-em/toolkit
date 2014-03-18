package net.tqft.toolkit.algebra.graphs

import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.modules.MapLinearCombo
import net.tqft.toolkit.algebra.modules.MapFreeModule
import net.tqft.toolkit.algebra.modules.Module

sealed trait VertexType[W]
case class Internal[W](label: W) extends VertexType[W]
case class BoundaryPoint[W](index: Int) extends VertexType[W]

object VertexType {
  implicit def ordering[W: Ordering] = new Ordering[VertexType[W]] {
    override def compare(t1: VertexType[W], t2: VertexType[W]) = {
      (t1, t2) match {
        case (BoundaryPoint(i1), BoundaryPoint(i2)) => i1 - i2
        case (BoundaryPoint(_), Internal(_)) => -1
        case (Internal(l1), Internal(l2)) => implicitly[Ordering[W]].compare(l1, l2)
        case (Internal(_), BoundaryPoint(_)) => 1
      }
    }
  }
}

object PlanarGraphWithBoundary {
  implicit def lift[W: Ordering](graph: ColouredGraph[VertexType[W]]): PlanarGraphWithBoundary[W] = PlanarGraphWithBoundary(???, ???, graph.adjacencies, graph.vertices.collect({ case Internal(l) => l }))

  def vertex[W: Ordering](n: Int, label: W) = {
    import net.tqft.toolkit.arithmetic.Mod._
    val adjacencies = (for (i <- 0 until n) yield Seq(i - 1 mod n, i + 1 mod n, n)) :+ (0 until n)
    val vertices: IndexedSeq[VertexType[W]] = (IndexedSeq.tabulate(n)(BoundaryPoint.apply[W]) :+ Internal(label)).toIndexedSeq
    PlanarGraphWithBoundary(n, 1, adjacencies, vertices)
  }
}

trait Spider[A] {
  def rotate(a: A, k: Int): A
  def tensor(a1: A, a2: A): A
  def stitch(a: A): A
}

trait LinearSpider[A, R] extends Spider[A] with Module[Map[A, R], R]

object Spider {
  implicit def lift[A: Spider, R: Ring]: Spider[Map[A, R]] = {
    ???
  }

  implicit def graphSpider[W: Ordering]: Spider[PlanarGraphWithBoundary[W]] = {
    new Spider[PlanarGraphWithBoundary[W]] {
      override def rotate(graph: PlanarGraphWithBoundary[W], k: Int) = {
        import net.tqft.toolkit.collections.Rotate._
        val colours: IndexedSeq[VertexType[W]] = ((0 until graph.numberOfBoundaryPoints).rotateLeft(k).map(BoundaryPoint.apply[W]) ++ graph.labels.map(Internal.apply)).toIndexedSeq
        PlanarGraphWithBoundary.lift(graph.colour(colours))
      }
      override def tensor(graph1: PlanarGraphWithBoundary[W], graph2: PlanarGraphWithBoundary[W]) = {
        ???
      }
      override def stitch(graph: PlanarGraphWithBoundary[W]) = {
        ???
      }
    }
  }
}

case class PlanarGraphWithBoundary[W: Ordering](numberOfBoundaryPoints: Int, numberOfInternalVertices: Int, adjacencies: IndexedSeq[Seq[Int]], labels: IndexedSeq[W]) extends ColouredGraph[VertexType[W]] {
  override val ordering = VertexType.ordering[W]
  override def numberOfVertices = numberOfBoundaryPoints + numberOfInternalVertices
  override def vertices = (IndexedSeq.tabulate(numberOfBoundaryPoints)(BoundaryPoint.apply[W]) ++ labels.map(Internal.apply)).toIndexedSeq

}
