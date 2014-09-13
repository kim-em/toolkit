package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.algebra.graphs.Graph
import net.tqft.toolkit.permutations.Permutations

case class TriangleFreeGraph(numberOfVertices: Int, adjacencies: IndexedSeq[Seq[Int]]) extends Graph with CanonicalGenerationWithIsomorphism[TriangleFreeGraph, IndexedSeq[Int]] { tfg =>
  import net.tqft.toolkit.permutations.Permutations._
  import net.tqft.toolkit.algebra.graphs._

//  override def toString = "TriangleFreeGraph(" + numberOfVertices + ", " + adjacencies + ")"

  override lazy val automorphisms = Dreadnaut.automorphismGroup(this)
  
  override def isomorphs: Iterator[TriangleFreeGraph] = {
    for(p <- Permutations.of(numberOfVertices)) yield copy(adjacencies = relabel(p).adjacencies)
  }

  override def findIsomorphismTo(other: TriangleFreeGraph) = {
    Permutations.of(numberOfVertices).find(p => relabel(p) == other)
  }
  
  override val ordering: Ordering[lowerObjects.Orbit] = Ordering.by({ o: lowerObjects.Orbit =>
    Dreadnaut.canonicalize(
      this.mark(Seq(o.representative.k)))
  })

  case class Upper(independentVertices: Seq[Int]) {
    lazy val result = TriangleFreeGraph(numberOfVertices + 1, addVertex(independentVertices).adjacencies)
    def inverse = result.Lower(numberOfVertices)
  }
  case class Lower(k: Int) {
    lazy val result = TriangleFreeGraph(numberOfVertices - 1, deleteVertex(k).adjacencies)
  }

  override def upperObjects: automorphisms.Action[Upper] = new automorphisms.Action[Upper] {
    override def elements = {
      // find independent sets
      def independentSubsetsOf(vertices: List[Int]): Iterator[List[Int]] = {
        vertices match {
          case Nil => Iterator(Nil)
          case i :: t => {
            independentSubsetsOf(t) ++ independentSubsetsOf(t.filterNot(adjacencies(i).contains)).map(i :: _)
          }
        }
      }
      for (s <- independentSubsetsOf((0 until numberOfVertices).toList).toStream) yield Upper(s)
    }
    override def act(g: IndexedSeq[Int], upper: Upper) = {
      val h = g.inverse
      Upper(upper.independentVertices.map(h).sorted)
    }
  }
  override lazy val lowerObjects = new automorphisms.Action[Lower] {
    override val elements = for (k <- (0 until numberOfVertices)) yield Lower(k)
    override def act(g: IndexedSeq[Int], lower: Lower) = {
      val h = g.inverse
      Lower(h(lower.k))
    }
  }
}