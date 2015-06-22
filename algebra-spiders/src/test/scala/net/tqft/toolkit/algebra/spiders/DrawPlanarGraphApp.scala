package net.tqft.toolkit.algebra.spiders

object DrawPlanarGraphApp extends App {
//  println(DrawPlanarGraph(PlanarGraph.crossing))
//  DrawPlanarGraph.withOutputPath("/Users/scott/scratch/").writePDF(PlanarGraph.crossing)("crossing.pdf")
//
//  {
//    val g = PlanarGraph.spider.tensor(PlanarGraph.crossing, PlanarGraph.strand)
//    println(g)
//    println(DrawPlanarGraph(g))
//    DrawPlanarGraph.withOutputPath("/Users/scott/scratch/").writePDF(g)("crossing-and-strand.pdf")
//  }

  println(DrawPlanarGraph(PlanarGraph.polygon(6)))
  DrawPlanarGraph.withOutputPath("/Users/scott/scratch/").withBoundaryWeight(1.4).writePDF(PlanarGraph.polygon(6))("hexagon.pdf")
//  DrawPlanarGraph.withOutputPath("/Users/scott/scratch/").createPDF(PlanarGraph.polygon(6))
}