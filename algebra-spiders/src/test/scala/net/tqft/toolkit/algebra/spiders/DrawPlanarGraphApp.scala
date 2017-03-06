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

//  println(DrawPlanarGraph(PlanarGraph.polygon(6)))
//  DrawPlanarGraph.withOutputPath("/Users/scott/scratch/").writePDF(PlanarGraph.polygon(6))("hexagon.pdf")
//  DrawPlanarGraph.withOutputPath("/Users/scott/scratch/").createPDF(PlanarGraph.polygon(6))
  
  DrawPlanarGraph.showPDF(PlanarGraph(16,Vector(List((18,16), (19,28), (7,17), (8,18), (9,19), (10,20)), Vector((11,21), (6,17), (15,16)), Vector((12,21), (7,18), (11,17)), Vector((13,21), (8,19), (12,18)), Vector((14,21), (9,20), (13,19)), Vector((15,21), (10,16), (14,20)), List((6,16), (19,17), (18,28))),Vector((1,0), (1,0), (1,0), (1,0), (1,0), (1,1)),0))
//  DrawPlanarGraph.showPDF(PlanarGraph.spider.tensor(PlanarGraph.spider.tensor(PlanarGraph.polygon(5), PlanarGraph.I), PlanarGraph.crossing))
  
//  DrawPlanarGraph.showPDF(PlanarGraph.spider.rotate(PlanarGraph.spider.tensor(PlanarGraph.polygon(5), PlanarGraph.I),0))
//  DrawPlanarGraph.showPDF(PlanarGraph.spider.rotate(PlanarGraph.spider.tensor(PlanarGraph.polygon(5), PlanarGraph.I),1))
//  DrawPlanarGraph.showPDF(PlanarGraph.spider.rotate(PlanarGraph.spider.tensor(PlanarGraph.polygon(5), PlanarGraph.I),2))
}