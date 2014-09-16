package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.spiders.TrivalentGraphs

object InnerProductsWithoutDodecahedra extends App {

  (for (n <- Iterator.from(5)) yield {
    (for (k <- Iterator.from(0)) yield {
      println(s"Computing inner products for D($n, $k)")
      try {
        val set = CubicSpider.innerProductMatrix(TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(n, k)).flatten.flatMap(_.variables).toSet -- Seq("d", "b", "t")
        println(set)
        set
      } catch {
        case e: java.lang.UnsupportedOperationException => Set("other variables...")
      }
    }).takeWhile(_.isEmpty)
  }).takeWhile(_.size > 0).size

}