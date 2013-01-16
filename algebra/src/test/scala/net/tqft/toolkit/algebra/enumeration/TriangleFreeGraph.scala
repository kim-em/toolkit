package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.algebra.graphs.Graph

case class TriangleFreeGraph(numberOfVertices: Int, edges: Set[Set[Int]]) extends Graph with CanonicalGeneration[TriangleFreeGraph, IndexedSeq[Int]] { tfg =>
  import net.tqft.toolkit.permutations.Permutations._
  import net.tqft.toolkit.algebra.graphs._
  override lazy val automorphisms = dreadnaut.automorphismGroup(this)
  override val ordering: Ordering[Lower] = Ordering.by({ l: Lower => dreadnaut.canonicalize(
//      l.result
      this.mark(Seq(l.k))
      ) })

  case class Upper(independentVertices: Set[Int]) {
    lazy val result = TriangleFreeGraph(numberOfVertices + 1, edges ++ independentVertices.map(Set(_, numberOfVertices)))
    def inverse = result.Lower(numberOfVertices)
  }
  case class Lower(k: Int) {
    lazy val result = TriangleFreeGraph(numberOfVertices - 1, edges.collect({ case e if !e.contains(k) => e.map({ case v if v > k => v - 1; case v => v }) }))
  }

  override def upperObjects = new automorphisms.Action[Upper] {
    override def elements = {
      // find independent sets
      def independentSubsetsOf(vertices: List[Int]): Iterator[List[Int]] = {
        vertices match {
          case Nil => Iterator(Nil)
          case head :: tail => {
            val connectedVertices = edges.filter(_.contains(head)).flatten
            independentSubsetsOf(tail) ++ independentSubsetsOf(tail.filterNot(connectedVertices.contains)).map(head :: _)
          }
        }
      }
      (for (s <- independentSubsetsOf(0 until numberOfVertices toList)) yield Upper(s.toSet)).toSet
    }
    override def act(g: IndexedSeq[Int], upper: Upper) = {
      val h = g.inverse
      Upper(upper.independentVertices.map(h))
    }
  }
  override def lowerObjects = new automorphisms.Action[Lower] {
    override def elements = (for (k <- 0 until numberOfVertices) yield Lower(k)).toSet
    override def act(g: IndexedSeq[Int], lower: Lower) = {
      val h = g.inverse
      Lower(h(lower.k))
    }
  }
}