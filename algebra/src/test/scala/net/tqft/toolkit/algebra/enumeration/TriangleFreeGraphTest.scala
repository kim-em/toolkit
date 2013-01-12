package net.tqft.toolkit.algebra.enumeration


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.graphs.Graph


@RunWith(classOf[JUnitRunner])
class TriangleFreeGraphTest extends FlatSpec with ShouldMatchers {

  // Giving the wrong answer! 11 instead of 14.
  val g0 = TriangleFreeGraph(1, Set.empty)
  val graphs = g0.descendants(_.numberOfVertices <= 5).toSeq
  for(g <- graphs) println(g)
  println(graphs.groupBy(_.numberOfVertices).mapValues(_.size).toSeq.sortBy(_._1))
  
  
}