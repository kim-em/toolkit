package net.tqft.toolkit.algebra.spiders

object DrawPlanarGraphApp extends App {
  println(DrawPlanarGraph(PlanarGraph.polygon(6)))
  DrawPlanarGraph.pdf("/Users/scott/scratch/hexagon.pdf", PlanarGraph.polygon(6))
}