package net.tqft.toolkit.algebra.enumeration

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.graphs.Graph

@RunWith(classOf[JUnitRunner])
class TriangleFreeGraphTest extends FlatSpec with ShouldMatchers {

  "descendants" should "enumerate all triangle free graphs with up to 8 vertices" in {
    val n = 10
    
    val g0 = TriangleFreeGraph(1, IndexedSeq(Seq()))
    val graphs = g0.descendants(n - _.numberOfVertices).toStream
    for (g <- graphs) println(g)
    graphs.groupBy(_.numberOfVertices).mapValues(_.size).toSeq.sortBy(_._1).map(_._2) should equal(List(1, 2, 3, 7, 14, 38, 107, 410, 1897, 12172, 105071, 1262180, 20797002, 467871369).take(n))
  }

}