package net.tqft.toolkit.algebra.spiders

object DrawPlanarGraphApp extends App {
  println(DrawPlanarGraph(PlanarGraph.polygon(6)))
  DrawPlanarGraph.withOutputPath("/Users/scott/scratch/").writePDF(PlanarGraph.polygon(6))("hexagon.pdf")
}