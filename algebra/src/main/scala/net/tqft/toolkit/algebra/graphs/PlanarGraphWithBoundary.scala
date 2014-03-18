package net.tqft.toolkit.algebra.graphs

sealed trait VertexType
case object Internal extends VertexType
sealed trait BoundaryVertexType extends VertexType
case object BoundaryPoint extends BoundaryVertexType
case object BoundaryBasePoint extends BoundaryVertexType

object VertexType {
  implicit object ordering extends Ordering[VertexType] {
    private def order(t: VertexType) = t match {
      case BoundaryBasePoint => 0
      case BoundaryPoint => 1
      case Internal => 2
    }
    override def compare(t1: VertexType, t2: VertexType) = {
      order(t1) - order(t2)
    }
  }
}

object PlanarGraphWithBoundary {
  implicit def lift[W: Ordering](graph: ColouredGraph[(VertexType, W)]) = PlanarGraphWithBoundary(???, ???, graph.adjacencies, graph.vertices)
}

case class PlanarGraphWithBoundary[W: Ordering](numberOfBoundaryPoints: Int, numberOfInternalVertices: Int, adjacencies: IndexedSeq[Seq[Int]], vertices: IndexedSeq[(VertexType, W)]) extends ColouredGraph[(VertexType, W)] {
  override val ordering = implicitly[Ordering[(VertexType, W)]]
  override def numberOfVertices = numberOfBoundaryPoints + numberOfInternalVertices
  
  def rotate(k: Int = 1): PlanarGraphWithBoundary[W] = {
    val boundaryColours: IndexedSeq[VertexType] = (Seq.fill(k)(BoundaryPoint)
      ++ Seq(BoundaryBasePoint)
      ++ Seq.fill(numberOfBoundaryPoints - k - 1)(BoundaryPoint)
      ++ Seq.fill(numberOfInternalVertices)(Internal)).toIndexedSeq
    val newColours = boundaryColours.zip(vertices.map(_._2))
    Dreadnaut.canonicalize(colour(newColours))
  }  
}