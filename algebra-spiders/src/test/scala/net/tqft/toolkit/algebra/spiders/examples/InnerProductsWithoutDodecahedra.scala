package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.spiders.TrivalentGraphs

object InnerProductsWithoutDodecahedra extends App {

  (for (n <- Iterator.from(5)) yield {
    (for (k <- Iterator.from(0)) yield {
      println(s"Computing inner products for D($n, $k)")
      try {
        val graphs = TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(n, k)
        val someGraphs = {
          val max = graphs.map(_.numberOfInternalVertices).max
          graphs.filter(g => g.numberOfInternalVertices + max >= 20)
        }
        val graphPairs = (for(g <- graphs; h <- graphs; k = g.numberOfInternalVertices + h.numberOfInternalVertices; if k >= 20) yield (g,h,k)).sortBy(t => -t._3).grouped(20)
        val innerProducts = for(chunk <- graphPairs; (g,h,_) <- chunk.par) yield {
          println((g,h))
          CubicSpider.evaluatedInnerProduct(Map(g -> 1), Map(h -> 1))
        }
        val set = innerProducts.flatMap(_.variables).toSet -- Seq("d", "b", "t")
        println(set)
        set
      } catch {
        case e: java.lang.UnsupportedOperationException => Set("other variables...")
      }
    }).takeWhile(_.isEmpty)
  }).takeWhile(_.size > 0).size

}