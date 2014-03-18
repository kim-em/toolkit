package net.tqft.toolkit.algebra.graphs

sealed trait VertexType[W]
case class Internal[W](label: W) extends VertexType[W]
sealed trait BoundaryVertexType[W] extends VertexType[W]
case class BoundaryPoint[W] extends BoundaryVertexType[W]
case class BoundaryBasePoint[W] extends BoundaryVertexType[W]

object VertexType {
  implicit def ordering[W:Ordering] = new Ordering[VertexType[W]] {
    private def order(t: VertexType[W]) = t match {
      case BoundaryBasePoint() => 0
      case BoundaryPoint() => 1
      case Internal(_) => 2
    }
    override def compare(t1: VertexType[W], t2: VertexType[W]) = {
      val r = order(t1) - order(t2)
      if(r != 0) {
        r
      } else {
        (t1, t2) match {
          case (Internal(l1), Internal(l2)) => implicitly[Ordering[W]].compare(l1, l2)
          case _ => 0
        }
      }
    }
  }
}

object PlanarGraphWithBoundary {
  implicit def lift[W: Ordering](graph: ColouredGraph[VertexType[W]]): PlanarGraphWithBoundary[W] = PlanarGraphWithBoundary(???, ???, graph.adjacencies, graph.vertices.collect({ case Internal(l) => l }))
  
  def vertex[W: Ordering](n: Int, label: W) = {
    import net.tqft.toolkit.arithmetic.Mod._
    val adjacencies = (for(i <- 0 until n) yield Seq(i - 1 mod n, i + 1 mod n, n)) :+ (0 until n)
    val vertices: IndexedSeq[VertexType[W]] = ((BoundaryBasePoint[W] +: Seq.fill(n-1)(BoundaryPoint[W])) :+ Internal(label)).toIndexedSeq
    PlanarGraphWithBoundary(n, 1, adjacencies, vertices)
  }
}

case class PlanarGraphWithBoundary[W: Ordering](numberOfBoundaryPoints: Int, numberOfInternalVertices: Int, adjacencies: IndexedSeq[Seq[Int]], labels: IndexedSeq[W]) extends ColouredGraph[VertexType[W]] {
  override val ordering = VertexType.ordering[W]
  override def numberOfVertices = numberOfBoundaryPoints + numberOfInternalVertices
  override def vertices = ((BoundaryBasePoint[W] +: Seq.fill(numberOfBoundaryPoints - 1)(BoundaryPoint[W])) ++ labels.map(Internal.apply)).toIndexedSeq
  
  def rotate(k: Int = 1): PlanarGraphWithBoundary[W] = {
    val colours: IndexedSeq[VertexType[W]] = (Seq.fill(k)(BoundaryPoint[W])
      ++ Seq(BoundaryBasePoint[W])
      ++ Seq.fill(numberOfBoundaryPoints - k - 1)(BoundaryPoint[W])
      ++ labels.map(Internal.apply)).toIndexedSeq
    PlanarGraphWithBoundary.lift(Dreadnaut.canonicalizeColouredGraph(colour(colours)))
  }  
}