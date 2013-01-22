package net.tqft.toolkit.algebra.graphs

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

@RunWith(classOf[JUnitRunner])
class dreadnautTest extends FlatSpec with ShouldMatchers {
  val C3 = Graph(3, IndexedSeq(Seq(1,2), Seq(0,2),Seq(0,1)))

  "automorphismGroup" should "compute all automorphisms of C_3" in {
    dreadnaut.automorphismGroup(C3).generators should equal(Set(List(0, 2, 1), List(1, 0, 2)))
  }
  "canonicalize" should "give the right answer for C_3" in {
    dreadnaut.canonicalize(C3).edges.toList should equal(List(Set(0, 1), Set(1, 2), Set(2, 0)))
  }
  "canonicalize" should "give the same answers for all relabellings of all graphs with 4 vertices" in {
    val n = 4
    for (g <- Graphs.onNVertices(n)) {
//      val h = g.mark(Seq(0))
      import net.tqft.toolkit.permutations.Permutations
      Permutations.of(n).map(p => dreadnaut.canonicalize(g.relabel(p))).toSet should have size (1)
    }
  }
  "canonicalize" should "identify isomorphic graphs" in {
    val g1 = Graph(4, IndexedSeq(Seq(), Seq(3), Seq(2), Seq(1,2)))
    val g2 = Graph(4, IndexedSeq(Seq(1), Seq(0,3), Seq(0), Seq(1)))
    dreadnaut.canonicalize(g1) should equal(dreadnaut.canonicalize(g2))
  }

}