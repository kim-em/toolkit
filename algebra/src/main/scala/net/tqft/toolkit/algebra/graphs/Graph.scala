package net.tqft.toolkit.algebra.graphs

import net.tqft.toolkit.permutations.Permutations

trait Graph {
  def edges: Traversable[Set[Int]]

  lazy val numberOfVertices = edges.flatten.toSet.size 
  
  def toDreadnautString: String = {
    val vertices = edges.flatten.toSeq.distinct.sorted
    require(vertices == (0 until vertices.size), "vertices must be consecutively numbered from 0")

    def adjacentTo(i: Int) = for (e <- edges; if e.contains(i); j <- (e - i)) yield j

    "n=" + vertices.size + " g " + (for (i <- vertices) yield adjacentTo(i).mkString(" ")).mkString("", "; ", ". ")
  }
  
  def relabel(labels: IndexedSeq[Int]): Graph = {
    Graph(edges.map(_.map(labels)))
  }
}

object Graph {
  def apply(edges: Traversable[Set[Int]]) = {
    val _edges = edges
    new Graph {
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
    ColouredGraph(super.relabel(labels).edges, labels.inverse.permute(vertices))
  }
}

object ColouredGraph {
  def apply[V](edges: Traversable[Set[Int]], vertices: IndexedSeq[V]) = {
    val _edges = edges
    val _vertices = vertices
    new ColouredGraph[V] {
      override def edges = _edges
      override def vertices = _vertices
    }
  }
}