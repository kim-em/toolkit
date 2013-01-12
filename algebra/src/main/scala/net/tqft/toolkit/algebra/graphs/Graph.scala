package net.tqft.toolkit.algebra.graphs

import net.tqft.toolkit.permutations.Permutations

trait Graph {
  def numberOfVertices: Int
  def edges: Traversable[Set[Int]]

  def toDreadnautString: String = {
    def adjacentTo(i: Int) = for (e <- edges; if e.contains(i); j <- (e - i)) yield j
    "n=" + numberOfVertices + " g " + (for (i <- 0 until numberOfVertices) yield adjacentTo(i).mkString(" ")).mkString("", "; ", ". ")
  }
  
  def relabel(labels: IndexedSeq[Int]): Graph = {
    Graph(numberOfVertices, edges.map(_.map(labels)))
  }
}

object Graph {
  def apply(numberOfVertices: Int, edges: Traversable[Set[Int]]) = {
    val _numberOfVertices = numberOfVertices
    val _edges = edges
    new Graph {
      override def numberOfVertices = _numberOfVertices
      override def edges = _edges}
    }
  }

trait ColouredGraph[V] extends Graph {
  def vertices: IndexedSeq[V]

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