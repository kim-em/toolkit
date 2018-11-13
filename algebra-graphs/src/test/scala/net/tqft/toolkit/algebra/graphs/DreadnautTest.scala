package net.tqft.toolkit.algebra.graphs

import org.scalatest._

class DreadnautTest extends FlatSpec with Matchers {
  val C3 = Graph(3, IndexedSeq(Seq(1, 2), Seq(0, 2), Seq(0, 1)))

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
  "dreadnaut" should "findIsomorphisms" in {
    val g = ColouredGraph(141, Vector(List(5), List(6), List(7), List(8), List(9), List(10), List(11), List(12), List(13), List(14), List(0), List(1), List(2), List(3), List(4), List(0, 5, 10), List(0, 5, 11), List(0, 5, 12), List(0, 5, 13), List(0, 5, 14), List(0, 6, 10), List(0, 6, 11), List(0, 6, 12), List(0, 6, 13), List(0, 6, 14), List(0, 7, 10), List(0, 7, 11), List(0, 7, 12), List(0, 7, 13), List(0, 7, 14), List(0, 8, 10), List(0, 8, 11), List(0, 8, 12), List(0, 8, 13), List(0, 8, 14), List(0, 9, 10), List(0, 9, 11), List(0, 9, 12), List(0, 9, 13), List(0, 9, 14), List(1, 5, 10), List(1, 5, 11), List(1, 5, 12), List(1, 5, 13), List(1, 5, 14), List(1, 6, 10), List(1, 6, 11), List(1, 6, 12), List(1, 6, 13), List(1, 6, 14), List(1, 7, 10), List(1, 7, 11), List(1, 7, 12), List(1, 7, 13), List(1, 7, 14), List(1, 8, 10), List(1, 8, 11), List(1, 8, 12), List(1, 8, 13), List(1, 8, 14), List(1, 9, 10), List(1, 9, 11), List(1, 9, 12), List(1, 9, 13), List(1, 9, 14), List(2, 5, 10), List(2, 5, 11), List(2, 5, 12), List(2, 5, 13), List(2, 5, 14), List(2, 6, 10), List(2, 6, 11), List(2, 6, 12), List(2, 6, 13), List(2, 6, 14), List(2, 7, 10), List(2, 7, 11), List(2, 7, 12), List(2, 7, 13), List(2, 7, 14), List(2, 8, 10), List(2, 8, 11), List(2, 8, 12), List(2, 8, 13), List(2, 8, 14), List(2, 9, 10), List(2, 9, 11), List(2, 9, 12), List(2, 9, 13), List(2, 9, 14), List(3, 5, 10), List(3, 5, 11), List(3, 5, 12), List(3, 5, 13), List(3, 5, 14), List(3, 6, 10), List(3, 6, 11), List(3, 6, 12), List(3, 6, 13), List(3, 6, 14), List(3, 7, 10), List(3, 7, 11), List(3, 7, 12), List(3, 7, 13), List(3, 7, 14), List(3, 8, 10), List(3, 8, 11), List(3, 8, 12), List(3, 8, 13), List(3, 8, 14), List(3, 9, 10), List(3, 9, 11), List(3, 9, 12), List(3, 9, 13), List(3, 9, 14), List(4, 5, 10), List(4, 5, 11), List(4, 5, 12), List(4, 5, 13), List(4, 5, 14), List(4, 6, 10), List(4, 6, 11), List(4, 6, 12), List(4, 6, 13), List(4, 6, 14), List(4, 7, 10), List(4, 7, 11), List(4, 7, 12), List(4, 7, 13), List(4, 7, 14), List(4, 8, 10), List(4, 8, 11), List(4, 8, 12), List(4, 8, 13), List(4, 8, 14), List(4, 9, 10), List(4, 9, 11), List(4, 9, 12), List(4, 9, 13), List(4, 9, 14), Vector()), Vector(-3, -3, -3, -3, -3, -2, -2, -2, -2, -2, -1, -1, -1, -1, -1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0))
    val h = ColouredGraph(141, Vector(List(63), List(6), List(7), List(8), List(9), List(10), List(11), List(12), List(13), List(14), List(0), List(1), List(2), List(3), List(4), List(0, 5, 10), List(0, 5, 11), List(0, 5, 12), List(0, 5, 13), List(0, 5, 14), List(0, 6, 10), List(0, 6, 11), List(0, 6, 12), List(0, 6, 13), List(0, 6, 14), List(0, 7, 10), List(0, 7, 11), List(0, 7, 12), List(0, 7, 13), List(0, 7, 14), List(0, 8, 10), List(0, 8, 11), List(0, 8, 12), List(0, 8, 13), List(0, 8, 14), List(0, 9, 10), List(0, 9, 11), List(0, 9, 12), List(0, 9, 13), List(0, 9, 14), List(1, 5, 10), List(1, 5, 11), List(1, 5, 12), List(1, 5, 13), List(1, 5, 14), List(1, 6, 10), List(1, 6, 11), List(1, 6, 12), List(1, 6, 13), List(1, 6, 14), List(1, 7, 10), List(1, 7, 11), List(1, 7, 12), List(1, 7, 13), List(1, 7, 14), List(1, 8, 10), List(1, 8, 11), List(1, 8, 12), List(1, 8, 13), List(1, 8, 14), List(1, 9, 10), List(1, 9, 11), List(1, 9, 12), List(1, 9, 13), List(1, 9, 14), List(2, 5, 10), List(2, 5, 11), List(2, 5, 12), List(2, 5, 13), List(2, 5, 14), List(2, 6, 10), List(2, 6, 11), List(2, 6, 12), List(2, 6, 13), List(2, 6, 14), List(2, 7, 10), List(2, 7, 11), List(2, 7, 12), List(2, 7, 13), List(2, 7, 14), List(2, 8, 10), List(2, 8, 11), List(2, 8, 12), List(2, 8, 13), List(2, 8, 14), List(2, 9, 10), List(2, 9, 11), List(2, 9, 12), List(2, 9, 13), List(2, 9, 14), List(3, 5, 10), List(3, 5, 11), List(3, 5, 12), List(3, 5, 13), List(3, 5, 14), List(3, 6, 10), List(3, 6, 11), List(3, 6, 12), List(3, 6, 13), List(3, 6, 14), List(3, 7, 10), List(3, 7, 11), List(3, 7, 12), List(3, 7, 13), List(3, 7, 14), List(3, 8, 10), List(3, 8, 11), List(3, 8, 12), List(3, 8, 13), List(3, 8, 14), List(3, 9, 10), List(3, 9, 11), List(3, 9, 12), List(3, 9, 13), List(3, 9, 14), List(4, 5, 10), List(4, 5, 11), List(4, 5, 12), List(4, 5, 13), List(4, 5, 14), List(4, 6, 10), List(4, 6, 11), List(4, 6, 12), List(4, 6, 13), List(4, 6, 14), List(4, 7, 10), List(4, 7, 11), List(4, 7, 12), List(4, 7, 13), List(4, 7, 14), List(4, 8, 10), List(4, 8, 11), List(4, 8, 12), List(4, 8, 13), List(4, 8, 14), List(4, 9, 10), List(4, 9, 11), List(4, 9, 12), List(4, 9, 13), List(4, 9, 14), Vector()), Vector(-3, -3, -3, -3, -3, -2, -2, -2, -2, -2, -1, -1, -1, -1, -1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0))

    val isomorphisms = Dreadnaut.findIsomorphisms(Seq(g, g, h))
    isomorphisms.map(_._1) should equal(Seq(0, 0, 2))
  }
}
