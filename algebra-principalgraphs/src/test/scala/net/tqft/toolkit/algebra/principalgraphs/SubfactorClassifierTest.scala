package net.tqft.toolkit.algebra.principalgraphs

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.math._

@RunWith(classOf[JUnitRunner])
class SubfactorClassifierTest extends FlatSpec with Matchers {
  import SubfactorClassifier._

  def p(w: SubfactorWeed) = {
    println(w)
    //          w.verifyAncestry should be (true)
  }

  "children" should "produce pairwise non-isomorphic outputs" in {
    val weed = SubfactorWeed(4.94, PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v0x1p1x0p1x0p0x1v1x0x0x0p0x0x1x0p0x0x0x1p0x1x0x0vduals1v1v1x2v1x2x4x3v", "bwd1v1v1v1p1v1x0p1x0v1x0p0x1v0x1p0x1p1x0p1x0vduals1v1v1x2v1x2v"))
    val graphs = weed.children
    val duplicates = graphs.groupBy(_.pair.invariant).filter(_._2.size > 1).map(_._2)
    duplicates should be('empty)
  }

  "descendantsTree" should "produce pairwise non-isomorphic outputs" in {
    val graphs = (for ((g, children) <- SubfactorWeed(4.94, PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v0x1p1x0p1x0p0x1v1x0x0x0p0x0x1x0p0x0x0x1p0x1x0x0duals1v1v1x2v1x2x4x3", "bwd1v1v1v1p1v1x0p1x0v1x0p0x1v0x1p0x1p1x0p1x0duals1v1v1x2v1x2")).descendantsTree()) yield {
      p(g)
      g
    }).toSeq
    val duplicates = graphs.groupBy(_.pair.invariant).filter(_._2.size > 1).map(_._2)
    duplicates should be('empty)
  }

  {
    val L = 4.86

    "verifyAncestry" should "check hereditary" in {
      val g = SubfactorWeed(L, PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v0x1p1x0v1x0p1x0p0x1duals1v1v1x2v1x2", "bwd1v1v1v1p1v1x0p0x1v1x0p0x1p1x0p0x1v0x0x1x0p0x1x0x0p0x0x0x1duals1v1v1x2v1x2x4x3"))
      for (a <- g.ancestry) {
        println(a)
      }
      g.verifyAncestry should be(true)
    }
  }


//  {
//    val L = 5.01
//    val graphs1 = (for ((g, children) <- A3(L).descendantsTreeFiltered(3, -1, -1, ignoring)) yield {
//      g
//    }).toList
//    val graphs2 = {
//      SubfactorWeed.experimental = true
//      val r = (for ((g, children) <- A3(L).descendantsTreeFiltered(3, -1, -1, ignoring)) yield {
//        g
//      }).toList
//      SubfactorWeed.experimental = false
//      r
//    }
//
//    "SubfactorWeed" should "produce the same results even as we make experimental modifications to the lower object choice function (2)" in {
//
//      graphs1.map(_.pair.invariant).toSet.size should equal(graphs1.size)
//      graphs2.map(_.pair.invariant).toSet.size should equal(graphs2.size)
//
//      val i1 = graphs1.map(g => (g.pair.invariant, g)).toMap
//      val i2 = graphs2.map(g => (g.pair.invariant, g)).toMap
//
//      val diff1 = (graphs1.map(_.pair.invariant).toSet -- graphs2.map(_.pair.invariant)).map(i1)
//      val diff2 = (graphs2.map(_.pair.invariant).toSet -- graphs1.map(_.pair.invariant)).map(i2)
//      println("stuff that only appears in standard mode:")
//      for (g <- diff1) {
//        println(g)
//      }
//      println("stuff that only appears in experimental mode:")
//      for (g <- diff2) {
//        println(g)
//      }
//      diff1 should be('empty)
//      diff2 should be('empty)
//    }
//  }
  {
    val graphs3 = (for ((g, children) <- A3(5.01).descendantsTreeFiltered(3, -1, -1, ignoring)) yield {
      g
    }).toSeq

    "descendantsTreeFiltered" should "produce pairwise non-isomorphic outputs" in {
      graphs3.map(_.pair.invariant).toSet.size should equal(graphs3.size)
    }
    "descendantsTreeFiltered" should "produce the right number of graphs" in {
      graphs3.size should equal(1054)
    }
  }
}