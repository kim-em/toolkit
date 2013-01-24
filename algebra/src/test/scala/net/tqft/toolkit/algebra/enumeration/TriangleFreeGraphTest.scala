package net.tqft.toolkit.algebra.enumeration

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.graphs.Graph

@RunWith(classOf[JUnitRunner])
class TriangleFreeGraphTest extends FlatSpec with ShouldMatchers {

  "upperObjects" should "correctly partition into orbits" in {
    TriangleFreeGraph(6, Vector(List(1, 5), List(0, 4), List(3), List(2), List(1, 5), List(0, 4))).verifyUpperOrbits.forall(_ == true) should be (true)
  }
  
  "children" should "be pairwaise non-isomorphic" in {
    def isomorphic_?(a: TriangleFreeGraph, b: TriangleFreeGraph) = {
      import net.tqft.toolkit.permutations.Permutations
      a.numberOfVertices == b.numberOfVertices && {
        Permutations.of(a.numberOfVertices).exists(p => a.relabel(p) == b)
      }
    }

    val g = TriangleFreeGraph(6, Vector(List(1, 5), List(0, 4), List(3), List(2), List(1, 5), List(0, 4)))
    for (s <- g.children.toSet.subsets(2)) {
      val a :: b :: Nil = s.toList
      isomorphic_?(a, b) should be(false)
    }
  }

  "descendants" should "enumerate all triangle free graphs with up to 8 vertices" in {
    val n = 8

    val g0 = TriangleFreeGraph(1, IndexedSeq(Seq()))
    val graphs = g0.descendants(n - _.numberOfVertices).toStream
//    for (g <- graphs) println(g)
    graphs.groupBy(_.numberOfVertices).mapValues(_.size).toSeq.sortBy(_._1).map(_._2) should equal(List(1, 2, 3, 7, 14, 38, 107, 410, 1897, 12172, 105071, 1262180, 20797002, 467871369).take(n))
  }

}