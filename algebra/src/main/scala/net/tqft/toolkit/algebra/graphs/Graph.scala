package net.tqft.toolkit.algebra.graphs

trait Graph {
  def edges: Traversable[Set[Int]]

  def toDreadnautString: String = {
    val vertices = edges.flatten.toSeq.sorted
    require(vertices == (0 until vertices.size))

    def adjacentTo(i: Int) = for (e <- edges; if e.contains(i); j <- (e - i)) yield j

    "n=" + vertices.size + " g\n" + (for (i <- vertices) yield adjacentTo(i).mkString(" ")).mkString(";\n")
  }
}

trait ColouredGraph[V] extends Graph {
  def vertices: IndexedSeq[V]

  override def toDreadnautString: String = {
    super.toDreadnautString + "f=" + vertices.distinct.map(v => (for ((w, i) <- vertices.zipWithIndex; if v == w) yield i).mkString(",")).mkString("[", "|", "]") + "\n"
  }
}