package net.tqft.toolkit.algebra.graphs

import net.tqft.toolkit.permutations.Permutations._

trait Graph {
  def numberOfVertices: Int
  def adjacencies: IndexedSeq[Seq[Int]]

  //  for(i <- 0 until numberOfVertices; j <- adjacencies(i)) {
//    require(adjacencies(j).contains(i))
//  }
//  
//  for(r <- adjacencies; i <- r) {
//    require(i < numberOfVertices)
//  }

  override def toString = "Graph(" + numberOfVertices + ", " + adjacencies + ")"
  override def hashCode = (numberOfVertices, adjacencies).hashCode
  
  override def equals(other: Any) = {
    other match {
      case other: Graph => numberOfVertices == other.numberOfVertices && adjacencies == other.adjacencies
      case _ => false
    }
  }

  def edges = (for((s, i) <- adjacencies.iterator.zipWithIndex; j <- s) yield Set(i,j)).toSet
  
  def toDreadnautString: String = {
    "n=" + numberOfVertices + " g " + (for (i <- 0 until numberOfVertices) yield adjacencies(i).mkString(" ")).mkString("", "; ", ". ")
  }

  def relabel(labels: IndexedSeq[Int]): Graph = {
//    require(labels.size == numberOfVertices)
    val p = labels.inverse
    Graph(numberOfVertices, labels.permute(adjacencies.map(a => a.map(p).sorted)))
  }

  def mark(vertices: Seq[Int]): ColouredGraph[Boolean] = {
    colour(for (i <- 0 until numberOfVertices) yield vertices.contains(i))
  }

  def addVertex(neighbours: Seq[Int]): Graph = {
    Graph(numberOfVertices + 1, adjacencies.zipWithIndex.map({
      case (a, i) if neighbours.contains(i) => a :+ numberOfVertices
      case (a, _) => a
    }) :+ neighbours.sorted)
  }
  
  def deleteVertex(k: Int): Graph = {
    import net.tqft.toolkit.collections.Deleted._
    Graph(numberOfVertices - 1, adjacencies.map({ a =>
      a.collect({
        case m if m < k => m
        case m if m > k => m - 1
      })
    }).deleted(k).toIndexedSeq)
  }
  
  def colour[V: Ordering](colours: IndexedSeq[V]): ColouredGraph[V] = {
    ColouredGraph(numberOfVertices, adjacencies, colours)
  }
}

object Graph {
  def apply(numberOfVertices: Int, adjacencies: IndexedSeq[Seq[Int]]) = {
    val _numberOfVertices = numberOfVertices
    val _adjacencies = adjacencies
    new Graph {
      override def numberOfVertices = _numberOfVertices
      override def adjacencies = _adjacencies
    }
  }

  def fromEdges(numberOfVertices: Int, edges: Set[Set[Int]]): Graph = {
    val adjacencies = for(i <- 0 until numberOfVertices) yield {
      (edges.filter(_.contains(i)).flatten - i).toSeq.sorted
    }
    Graph(numberOfVertices, adjacencies)
  }
  
  implicit val ordering: Ordering[Graph] = {
    import net.tqft.toolkit.collections.LexicographicOrdering._
    import net.tqft.toolkit.collections.Orderings._
    Ordering.by({ g: Graph => g.numberOfVertices }).refineBy({ g: Graph => g.adjacencies })
  }

}

object Graphs {
  def cyclic(n: Int): Graph = {
    import net.tqft.toolkit.arithmetic.Mod._
    Graph(n, for (i <- 0 until n) yield Seq((i - 1) mod n, (i + 1) mod n))
  }
  def complete(n: Int): Graph = {
    Graph(n, for (i <- 0 until n) yield (0 until i) ++ (i + 1 until n))
  }
  def onNVertices(n: Int): Iterator[Graph] = {
	for(edges <- complete(n).edges.subsets) yield {
	  Graph.fromEdges(n, edges)
	}
  }
}

trait ColouredGraph[V] extends Graph {
  def vertices: IndexedSeq[V]

  implicit val ordering: Ordering[V]

  override def toString = "ColouredGraph(" + numberOfVertices + ", " + adjacencies + ", " + vertices + ")"
  override def hashCode = (numberOfVertices, adjacencies, vertices).hashCode
  override def equals(other: Any) = {
    other match {
      case other: ColouredGraph[_] => numberOfVertices == other.numberOfVertices && adjacencies == other.adjacencies && vertices == other.vertices
      case _ => false
    }
  }

  override def toDreadnautString: String = {
    super.toDreadnautString + " f=" + vertices.distinct.sorted.map(v => (for ((w, i) <- vertices.zipWithIndex; if v == w) yield i).mkString(",")).mkString("[", "|", "]")
  }

  override def relabel(labels: IndexedSeq[Int]): ColouredGraph[V] = {
    require(labels.size == numberOfVertices)
    import net.tqft.toolkit.permutations.Permutations._
    ColouredGraph(numberOfVertices, super.relabel(labels).adjacencies, labels.permute(vertices))
  }
}

object ColouredGraph {
  def apply[V: Ordering](numberOfVertices: Int, adjacencies: IndexedSeq[Seq[Int]], vertices: IndexedSeq[V]) = {
    val _numberOfVertices = numberOfVertices
    val _adjacencies = adjacencies
    val _vertices = vertices
    new ColouredGraph[V] {
      override def numberOfVertices = _numberOfVertices
      override def adjacencies = _adjacencies
      override def vertices = _vertices
      override val ordering = implicitly[Ordering[V]]
    }
  }

  implicit def ordering[V: Ordering]: Ordering[ColouredGraph[V]] = {
    import net.tqft.toolkit.collections.LexicographicOrdering._
    import net.tqft.toolkit.collections.Orderings._
    Ordering.by({ g: ColouredGraph[V] => g.vertices }).refineBy({ g: ColouredGraph[V] => g.adjacencies })
  }

}