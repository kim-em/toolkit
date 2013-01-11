package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.algebra.graphs.Graph

// An incomplete example, just to make sure the framework is usable. 
case class TriangleFreeGraph(numberOfVertices: Int, edges: Set[Set[Int]]) extends Graph with CanonicalGeneration[TriangleFreeGraph, IndexedSeq[Int], Graph] {
//  import net.tqft.toolkit.permutations.Permutations._
  import net.tqft.toolkit.algebra.graphs._
  override lazy val automorphisms = dreadnaut.automorphismGroup(this)
  override implicit val ordering: Ordering[Graph] = {
    import net.tqft.toolkit.collections.LexicographicOrdering._
    Ordering.by(_.edges.toList.map(_.toList.sorted).sorted)
  }
  override def invariant = dreadnaut.canonicalize(this)

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
//      val h = g.inverse
      Upper(upper.independentVertices.map(g))
    }
  }
  override def lowerObjects = new automorphisms.Action[Lower] {
    override def elements = (for (k <- 0 until numberOfVertices) yield Lower(k)).toSet
    override def act(g: IndexedSeq[Int], lower: Lower) = {
//      val h = g.inverse
      Lower(g(lower.k))
    }
  }
}