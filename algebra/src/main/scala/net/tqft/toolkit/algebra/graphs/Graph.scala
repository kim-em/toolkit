package net.tqft.toolkit.algebra.graphs

import net.tqft.toolkit.permutations.Permutations._

trait Graph {
  def numberOfVertices: Int
  def edges: Traversable[Set[Int]]

  override def toString = "Graph(" + numberOfVertices + ", " + edges + ")"
  override def hashCode = (numberOfVertices, edges).hashCode
  override def equals(other: Any) = {
    other match {
      case other: Graph => numberOfVertices == other.numberOfVertices && edges == other.edges
      case _ => false
    }
  }

  def toDreadnautString: String = {
    def adjacentTo(i: Int) = for (e <- edges; if e.contains(i); j <- (e - i)) yield j
    "n=" + numberOfVertices + " g " + (for (i <- 0 until numberOfVertices) yield adjacentTo(i).mkString(" ")).mkString("", "; ", ". ")
  }

  def relabel(labels: IndexedSeq[Int]): Graph = {
    val p = labels.inverse
    Graph(numberOfVertices, edges.map(_.map(p)))
  }

  def mark(vertices: Seq[Int]): ColouredGraph[Boolean] = {
    colour(for (i <- 0 until numberOfVertices) yield vertices.contains(i))
  }

  def colour[V](colours: IndexedSeq[V]): ColouredGraph[V] = {
    ColouredGraph(numberOfVertices, edges, colours)
  }
}

object Graph {
  def apply(numberOfVertices: Int, edges: Traversable[Set[Int]]) = {
    val _numberOfVertices = numberOfVertices
    val _edges = edges
    new Graph {
      override def numberOfVertices = _numberOfVertices
      override def edges = _edges
    }
  }

  implicit val ordering: Ordering[Graph] = {
    import net.tqft.toolkit.collections.LexicographicOrdering._
    import net.tqft.toolkit.collections.Orderings._
    Ordering.by({ g: Graph => g.numberOfVertices }).refineBy({ g: Graph => g.edges.toSeq.map(_.toSeq.sorted).sorted })
  }

}

object Graphs {
  def cyclic(n: Int): Graph = {
    Graph(n, for (i <- 0 until n) yield Set(i, (i + 1) % n))
  }
  def complete(n: Int): Graph = {
    Graph(n, for (i <- 0 until n; j <- i + 1 until n) yield Set(i, j))
  }
  def onNVertices(n: Int): Iterator[Graph] = {
    import net.tqft.toolkit.collections.Subsets._
    val allEdges = for (i <- 0 until n; j <- i + 1 until n) yield Set(i, j)
    for (edges <- allEdges.subsets) yield Graph(n, edges)
  }
}

trait ColouredGraph[V] extends Graph {
  def vertices: IndexedSeq[V]

  override def toString = "ColouredGraph(" + numberOfVertices + ", " + edges + ", " + vertices + ")"
  override def hashCode = (numberOfVertices, edges, vertices).hashCode
  override def equals(other: Any) = {
    other match {
      case other: ColouredGraph[_] => numberOfVertices == other.numberOfVertices && edges == other.edges && vertices == other.vertices
      case _ => false
    }
  }

  override def toDreadnautString: String = {
    super.toDreadnautString + " f=" + vertices.distinct.map(v => (for ((w, i) <- vertices.zipWithIndex; if v == w) yield i).mkString(",")).mkString("[", "|", "]")
  }

  override def relabel(labels: IndexedSeq[Int]): ColouredGraph[V] = {
    import net.tqft.toolkit.permutations.Permutations._
    ColouredGraph(numberOfVertices, super.relabel(labels).edges, labels.inverse.permute(vertices))
  }
}

object ColouredGraph {
  def apply[V](numberOfVertices: Int, edges: Traversable[Set[Int]], vertices: IndexedSeq[V]) = {
    val _numberOfVertices = numberOfVertices
    val _edges = edges
    val _vertices = vertices
    new ColouredGraph[V] {
      override def numberOfVertices = _numberOfVertices
      override def edges = _edges
      override def vertices = _vertices
    }
  }
}