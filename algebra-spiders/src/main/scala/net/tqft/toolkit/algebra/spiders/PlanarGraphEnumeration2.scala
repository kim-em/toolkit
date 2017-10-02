package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import net.tqft.toolkit.Logging

import net.tqft.toolkit.arithmetic.Mod._

case class PlanarGraphEnumerationContext2(
    vertices: Seq[VertexType],
    forbiddenSubgraphs: Seq[PlanarGraph],
    maximumVertices: Option[Int] = None,
    maximumBoundaryPoints: Option[Int] = None) extends Logging {
  val spider = PlanarGraph.spider
  val largestVertex = vertices.map(_.perimeter).max

  /* 
   * We think of the collection of all connected planar graphs as a directed graph, 
   * with edges corresponding to gluing on a vertex.
   * Every connected planar graph is a descendant of some single vertex graph (and possibly the descendant of several such).
   * 
   * We then try to come up with a condition on the edges, giving a directed subgraph, such that
   * every connected planar graph is still the descendant of a (hopefully just one) single vertex graph,
   * and the subgraph is either a tree, or at least very close to being one!
   * 
   * As usual, we follow Brendan's philosophy that it's easiest to think about putting a condition on the _inverse_ edges,
   * that is, ways to delete a vertex which touches the boundary.
   * 
   * We propose the following rules:
   * 1) never delete a vertex straddling the marked point (because we know from experience these cases are confusing!)
   * 2) never delete a vertex which would disconnect the graph
   * 3) prefer to delete the dangliest vertex
   * 3.5) ???
   * 4) if there's still a tie, break it via a canonical labelling of the graph.
   * 
   * These rules are designed so that every graph G has a unique allowed parent p(G).
   * We only generate those children c of a graph G such that p(c) = G.
   * 
   * In particular, we never need to glue on a vertex across the marked point.
   * 
   * We can use facts such as:
   * Any n-valent vertex that we glue on via k > 0 edges has 0-dangliness n-k.
   * If any existing vertex has 0-dangliness strictly small, we have to glue over the top of it! 
   */

  def parent(p: PlanarGraph): PlanarGraph = parent_(p).get._2.apply()

  def parent_(p: PlanarGraph): Option[(Int, () => PlanarGraph)] = {
    //    if(p.numberOfBoundaryPoints < 2) {
    //      println(p)
    //      DrawPlanarGraph.showPDF(p)
    //    }

    assert(p.numberOfBoundaryPoints >= 2)
    assert(p.numberOfInternalVertices != 0)
    if (p.numberOfInternalVertices == 1) {
      None
    } else {
      val boundaryVertices = p.neighboursOf(0)
      // // Actually, there's no need to do this step; our implementation of disconnectingVertices picks up this case.      
      //      val vertexStraddlingMarkedPoint = {
      //        if(boundaryVertices.head == boundaryVertices.last) {
      //          Some(boundaryVertices.head)
      //        } else {
      //          None
      //        }
      //      }

      val disconnectingVertices = {
        import net.tqft.toolkit.collections.Split._
        import net.tqft.toolkit.collections.Tally._
        import net.tqft.toolkit.collections.Rotate._
        val verticesVisibleFromBoundaryFaces = p.vertexFlags(0).map(_._2).map(i => p.faceBoundary(i).ensuring(_.size == 1).head.map(_._1)).map(s => s.rotateLeft(s.indexOf(0)).tail.reverse)
//        println("verticesVisibleFromBoundaryFaces = " + verticesVisibleFromBoundaryFaces)
        val result = verticesVisibleFromBoundaryFaces.flatten.rle.map(_._1).tally.collect({ case (v, k) if k > 1 => v }).toSet
//        println("disconnectingVertices = " + result)
        result
      }

      val candidateVertices = {
        if (boundaryVertices.distinct.size == 1) {
          // if only one vertex touches the boundary, we incorrectly count it as disconnecting
          boundaryVertices.distinct
        } else {
          boundaryVertices.distinct.filter(v => !disconnectingVertices.contains(v))
        }
      }

      def dangliest(vertices: Seq[Int], dangliness: Seq[Seq[Int]] = p.dangliness.take(3)): Seq[Int] = {
        assert(vertices.nonEmpty, "dangliest called with no vertices!\n" + p)
        dangliness match {
          case h +: t => {
            val m = vertices.map(i => h(i)).max
            vertices.filter(i => h(i) == m)
          }
          case _ => vertices
        }
      }

      val dangliestVertices = dangliest(candidateVertices)
      // Next, we delete the dangliest of the candidateVertices, and then tie-break using Nauty.

      if (dangliestVertices.size == 1) {
        val v = dangliestVertices.head
        Some((v, () => p.deleteBoundaryVertex(v)))
      } else {
        val deletions = dangliestVertices.map(v => p.deleteBoundaryVertex(v).canonicalFormWithDefect._1)
        val choice = deletions.zip(dangliestVertices).min
        Some((choice._2, () => choice._1))
      }
    }
  }

  def raw_children(p: PlanarGraph): Seq[PlanarGraph] = {
    maximumVertices.map(mv => {
      if (p.numberOfInternalVertices >= mv) return Nil
      maximumBoundaryPoints.map(mb => if (p.numberOfBoundaryPoints >= mb + (mv - p.numberOfInternalVertices) * (largestVertex - 1)) return Nil)
    })

    // As a first approximation, generate all children that don't straddle the marked point, and then check if they have the right parent.
    val graphs = for (
      v <- vertices;
      r <- 0 until v.allowedRotationStep;
      k <- 1 until v.perimeter;
      if p.numberOfBoundaryPoints + v.perimeter - 2 * k >= 2; // don't both producing things with < 2 boundary points.
      i <- 0 to p.numberOfBoundaryPoints - k;
      result = spider.rotate(spider.multiply(spider.rotate(PlanarGraph.star(v), r), spider.rotate(p, -i), k), i)
    ) yield result
    // TODO After implementing that, as an optimisation use dangliness to be a bit cleverer about which vertices to add.

    // TODO this could be much more efficient
    graphs.filter(g => forbiddenSubgraphs.forall(f => !g.subgraphs(f).excisions.hasNext))
  }

  def children(p: PlanarGraph): Seq[PlanarGraph] = {
    val p0 = p.canonicalFormWithDefect._1

    for (
      c <- raw_children(p0);
      if parent(c).canonicalFormWithDefect._1 == p0
    ) yield c

  }

  def children_without_duplicates(p: PlanarGraph): Seq[PlanarGraph] = {
    // TODO are there actually duplicates?
    children(p).map(g => g.canonicalFormWithDefect._1).distinct
  }

  def descendants(p: PlanarGraph): Iterator[PlanarGraph] = {
    Iterator(p) ++ children_without_duplicates(p).iterator.flatMap(descendants)
  }

  def verify_raw_child_of_parent(g: PlanarGraph): Boolean = {
    val g0 = g.canonicalFormWithDefect._1
    raw_children(parent(g0)).map(g => g.canonicalFormWithDefect._1).contains(g0)
  }

  def verify_child_of_parent(g: PlanarGraph): Boolean = {
    val g0 = g.canonicalFormWithDefect._1
    children_without_duplicates(parent(g0)).contains(g0)
  }

  def verify_ancestry(g: PlanarGraph): Boolean = {
    if (g.numberOfInternalVertices == 1) {
      true
    } else {
      verify_child_of_parent(g) && verify_ancestry(parent(g))
    }
  }
}
