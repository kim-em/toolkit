package net.tqft.toolkit.algebra.graphs

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class DreadnautTest extends FlatSpec with Matchers {
  val C3 = Graph(3, IndexedSeq(Seq(1, 2), Seq(0, 2), Seq(0, 1)))

  "dreadnaut" should "not choke on input" in {
    Dreadnaut.invokeDreadnaut("n=140 g 5 0; 6 1; 7 2; 8 3; 9 4; 10; 11; 12; 13; 14; 0; 1; 2; 3; 4; 0 5 10; 0 5 11; 0 5 12; 0 5 13; 0 5 14; 0 6 10; 0 6 11; 0 6 12; 0 6 13; 0 6 14; 0 7 10; 0 7 11; 0 7 12; 0 7 13; 0 7 14; 0 8 10; 0 8 11; 0 8 12; 0 8 13; 0 8 14; 0 9 10; 0 9 11; 0 9 12; 0 9 13; 0 9 14; 1 5 10; 1 5 11; 1 5 12; 1 5 13; 1 5 14; 1 6 10; 1 6 11; 1 6 12; 1 6 13; 1 6 14; 1 7 10; 1 7 11; 1 7 12; 1 7 13; 1 7 14; 1 8 10; 1 8 11; 1 8 12; 1 8 13; 1 8 14; 1 9 10; 1 9 11; 1 9 12; 1 9 13; 1 9 14; 2 5 10; 2 5 11; 2 5 12; 2 5 13; 2 5 14; 2 6 10; 2 6 11; 2 6 12; 2 6 13; 2 6 14; 2 7 10; 2 7 11; 2 7 12; 2 7 13; 2 7 14; 2 8 10; 2 8 11; 2 8 12; 2 8 13; 2 8 14; 2 9 10; 2 9 11; 2 9 12; 2 9 13; 2 9 14; 3 5 10; 3 5 11; 3 5 12; 3 5 13; 3 5 14; 3 6 10; 3 6 11; 3 6 12; 3 6 13; 3 6 14; 3 7 10; 3 7 11; 3 7 12; 3 7 13; 3 7 14; 3 8 10; 3 8 11; 3 8 12; 3 8 13; 3 8 14; 3 9 10; 3 9 11; 3 9 12; 3 9 13; 3 9 14; 4 5 10; 4 5 11; 4 5 12; 4 5 13; 4 5 14; 4 6 10; 4 6 11; 4 6 12; 4 6 13; 4 6 14; 4 7 10; 4 7 11; 4 7 12; 4 7 13; 4 7 14; 4 8 10; 4 8 11; 4 8 12; 4 8 13; 4 8 14; 4 9 10; 4 9 11; 4 9 12; 4 9 13; 4 9 14.  f=[0|1,2,3,4|5,6,7,8,9|10,11,12,13,14|16,17,18,19,20,22,23,24,25,26,28,29,30,31,32,34,35,36,37,38,40,42,43,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,76,77,78,79,80,81,82,84,85,86,87,88,90,91,92,94,95,96,97,98,99,100,101,102,104,106,108,109,110,111,112,113,115,116,117,118,120,121,122,123,124,125,126,127,128,130,131,132,133,136,139|15,21,27,33,39,41,45,67,75,89,93,105,119,129,135,137|114,134,138|83,103,107]cxb").size should equal(165)
  }
  
  "automorphismGroup" should "compute all automorphisms of C_3" in {
    Dreadnaut.automorphismGroup(C3).generators should equal(Seq(List(0, 2, 1), List(1, 0, 2)))
  }
  "canonicalize" should "give the right answer for C_3" in {
    Dreadnaut.canonicalize(C3).edges should equal(Set(Set(0, 1), Set(1, 2), Set(2, 0)))
  }
  "canonicalize" should "give the same answers for all relabellings of all graphs with 4 vertices" in {
    val n = 4
    for (g <- Graphs.onNVertices(n)) {
      import net.tqft.toolkit.permutations.Permutations
      Permutations.of(n).map(p => Dreadnaut.canonicalize(g.relabel(p))).toSet should have size (1)
    }
  }
  "canonicalize(g: ColouredGraph[])" should "give the same answers for all relabellings of all graphs with 4 vertices, 1 marked" in {
    val n = 4
    for (g <- Graphs.onNVertices(n)) {
      import net.tqft.toolkit.permutations.Permutations
      val h = g.mark(Seq(0))
      Permutations.preserving(0 +: Seq.fill(n - 1)(1)).map(p => Dreadnaut.canonicalize(h.relabel(p))).toSet should have size (1)
    }
  }
  "canonicalize" should "identify isomorphic graphs" in {
    val g1 = Graph(4, IndexedSeq(Seq(), Seq(3), Seq(3), Seq(1, 2)))
    val g2 = Graph(4, IndexedSeq(Seq(1), Seq(0, 3), Seq(), Seq(1)))
    Dreadnaut.canonicalize(g1) should equal(Dreadnaut.canonicalize(g2))
  }
  "dreadnaut" should "find orbits of vertices under the automorphism group" in {
    val g = Graph(5, IndexedSeq(Seq(1), Seq(0), Seq(3), Seq(4), Seq(2)))
    g.automorphismAction.allOrbits.size should equal(2)
    g.automorphismAction.allOrbits.toSeq.map(_.size).sorted should equal(Seq(2, 3))
  }

  "dreadnaut" should "find the right number of generators" in {
    val g = ColouredGraph(69, Vector(List(1), List(44, 2), List(3), List(46, 4, 5), List(6), List(7), List(49, 9, 10), List(50, 8, 11), List(14), List(15), List(11, 13), List(10, 12), List(53, 17, 18), List(54, 16, 20), List(55, 21), List(56, 19), List(25), List(24), List(19), List(18, 22), List(21), List(20, 23), List(59, 26, 30), List(60, 27, 32), List(61, 28, 33), List(62, 29, 31), List(27), List(26), List(29, 36), List(28, 37), List(31, 34), List(30), List(33, 35), List(32), List(65, 38), List(66, 38), List(67, 40, 42), List(68, 39, 41), List(), List(), List(), List(), List(), List(44), List(1, 45), List(46), List(3, 47, 48), List(49, 50), List(), List(6, 51), List(7, 52), List(53, 56), List(54, 55), List(12, 57), List(13, 58), List(14), List(15), List(58, 60, 62), List(57, 59, 61), List(22), List(23), List(24, 63), List(25, 64), List(66, 68), List(65, 67), List(34), List(35), List(36), List(37)), Vector((0, 12), (0, 11), (0, 10), (0, 9), (0, 8), (0, 8), (0, 7), (0, 7), (0, 6), (0, 6), (0, 6), (0, 6), (0, 5), (0, 5), (0, 5), (0, 5), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 3), (0, 3), (0, 3), (0, 3), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 1), (0, 1), (0, 1), (0, 1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (1, 12), (1, 11), (1, 10), (1, 9), (1, 8), (1, 8), (1, 7), (1, 7), (1, 6), (1, 6), (1, 5), (1, 5), (1, 5), (1, 5), (1, 4), (1, 4), (1, 3), (1, 3), (1, 3), (1, 3), (1, 2), (1, 2), (1, 1), (1, 1), (1, 1), (1, 1)))
    Dreadnaut.automorphismGroup(g).generators.size should equal(3)
  }
  "dreadnaut" should "report generators which are actually automorphisms" in {
    val g = ColouredGraph(69, Vector(List(1), List(44, 2), List(3), List(46, 4, 5), List(6), List(7), List(49, 9, 10), List(50, 8, 11), List(14), List(15), List(11, 13), List(10, 12), List(53, 17, 18), List(54, 16, 20), List(55, 21), List(56, 19), List(25), List(24), List(19), List(18, 22), List(21), List(20, 23), List(59, 26, 30), List(60, 27, 32), List(61, 28, 33), List(62, 29, 31), List(27), List(26), List(29, 36), List(28, 37), List(31, 34), List(30), List(33, 35), List(32), List(65, 38), List(66, 38), List(67, 40, 42), List(68, 39, 41), List(), List(), List(), List(), List(), List(44), List(1, 45), List(46), List(3, 47, 48), List(49, 50), List(), List(6, 51), List(7, 52), List(53, 56), List(54, 55), List(12, 57), List(13, 58), List(14), List(15), List(58, 60, 62), List(57, 59, 61), List(22), List(23), List(24, 63), List(25, 64), List(66, 68), List(65, 67), List(34), List(35), List(36), List(37)), Vector((0, 12), (0, 11), (0, 10), (0, 9), (0, 8), (0, 8), (0, 7), (0, 7), (0, 6), (0, 6), (0, 6), (0, 6), (0, 5), (0, 5), (0, 5), (0, 5), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 3), (0, 3), (0, 3), (0, 3), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 1), (0, 1), (0, 1), (0, 1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (1, 12), (1, 11), (1, 10), (1, 9), (1, 8), (1, 8), (1, 7), (1, 7), (1, 6), (1, 6), (1, 5), (1, 5), (1, 5), (1, 5), (1, 4), (1, 4), (1, 3), (1, 3), (1, 3), (1, 3), (1, 2), (1, 2), (1, 1), (1, 1), (1, 1), (1, 1)))
    for (h <- Dreadnaut.automorphismGroup(g).generators) {
      g.relabel(h) should equal(g.relabel(IndexedSeq.range(0, g.numberOfVertices)))
    }
  }
  "dreadnaut" should "report generators which are actually automorphisms (2)" in {
    val g = ColouredGraph(141, Vector(List(5), List(6), List(7), List(8), List(9), List(10), List(11), List(12), List(13), List(14), List(0), List(1), List(2), List(3), List(4), List(0, 5, 10), List(0, 5, 11), List(0, 5, 12), List(0, 5, 13), List(0, 5, 14), List(0, 6, 10), List(0, 6, 11), List(0, 6, 12), List(0, 6, 13), List(0, 6, 14), List(0, 7, 10), List(0, 7, 11), List(0, 7, 12), List(0, 7, 13), List(0, 7, 14), List(0, 8, 10), List(0, 8, 11), List(0, 8, 12), List(0, 8, 13), List(0, 8, 14), List(0, 9, 10), List(0, 9, 11), List(0, 9, 12), List(0, 9, 13), List(0, 9, 14), List(1, 5, 10), List(1, 5, 11), List(1, 5, 12), List(1, 5, 13), List(1, 5, 14), List(1, 6, 10), List(1, 6, 11), List(1, 6, 12), List(1, 6, 13), List(1, 6, 14), List(1, 7, 10), List(1, 7, 11), List(1, 7, 12), List(1, 7, 13), List(1, 7, 14), List(1, 8, 10), List(1, 8, 11), List(1, 8, 12), List(1, 8, 13), List(1, 8, 14), List(1, 9, 10), List(1, 9, 11), List(1, 9, 12), List(1, 9, 13), List(1, 9, 14), List(2, 5, 10), List(2, 5, 11), List(2, 5, 12), List(2, 5, 13), List(2, 5, 14), List(2, 6, 10), List(2, 6, 11), List(2, 6, 12), List(2, 6, 13), List(2, 6, 14), List(2, 7, 10), List(2, 7, 11), List(2, 7, 12), List(2, 7, 13), List(2, 7, 14), List(2, 8, 10), List(2, 8, 11), List(2, 8, 12), List(2, 8, 13), List(2, 8, 14), List(2, 9, 10), List(2, 9, 11), List(2, 9, 12), List(2, 9, 13), List(2, 9, 14), List(3, 5, 10), List(3, 5, 11), List(3, 5, 12), List(3, 5, 13), List(3, 5, 14), List(3, 6, 10), List(3, 6, 11), List(3, 6, 12), List(3, 6, 13), List(3, 6, 14), List(3, 7, 10), List(3, 7, 11), List(3, 7, 12), List(3, 7, 13), List(3, 7, 14), List(3, 8, 10), List(3, 8, 11), List(3, 8, 12), List(3, 8, 13), List(3, 8, 14), List(3, 9, 10), List(3, 9, 11), List(3, 9, 12), List(3, 9, 13), List(3, 9, 14), List(4, 5, 10), List(4, 5, 11), List(4, 5, 12), List(4, 5, 13), List(4, 5, 14), List(4, 6, 10), List(4, 6, 11), List(4, 6, 12), List(4, 6, 13), List(4, 6, 14), List(4, 7, 10), List(4, 7, 11), List(4, 7, 12), List(4, 7, 13), List(4, 7, 14), List(4, 8, 10), List(4, 8, 11), List(4, 8, 12), List(4, 8, 13), List(4, 8, 14), List(4, 9, 10), List(4, 9, 11), List(4, 9, 12), List(4, 9, 13), List(4, 9, 14), Vector()), Vector(-3, -3, -3, -3, -3, -2, -2, -2, -2, -2, -1, -1, -1, -1, -1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0))
   for (h <- Dreadnaut.automorphismGroup(g).generators) {
      g.relabel(h) should equal(g.relabel(IndexedSeq.range(0, g.numberOfVertices)))
    }
  }
  
  "dreadnaut" should "find isomorphisms" in {
    val g = Graph(3, IndexedSeq(Seq(), Seq(2), Seq(1)))
    val h = Graph(3, IndexedSeq(Seq(2), Seq(), Seq(0)))
    Dreadnaut.findIsomorphism(g, h) should equal(Some(IndexedSeq(1, 0, 2)))
  }

  "dreadnaut" should "find a cyclic group of automorphisms" in {
    val k = 5
    val g = Graph(4 * k, IndexedSeq.tabulate(k)({ i => IndexedSeq(Seq(1 + 4 * i, 2 + 4 * i), Seq(2 + 4 * i), Seq(3 + 4 * i, (4 + 4 * i) % (4 * k)), Seq()) }).flatten)
//    println(g)
//    for(x <- Dreadnaut.automorphismGroup(g).generators) {
//      println(x)
//      println(g.relabel(x))
//    }
    Dreadnaut.automorphismGroup(g).size should equal(k)
  }
}
