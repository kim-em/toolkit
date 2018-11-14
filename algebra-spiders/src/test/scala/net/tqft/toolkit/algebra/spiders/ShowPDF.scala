package net.tqft.toolkit.algebra.spiders

object ShowPDF extends App {
  DrawPlanarGraph.showPDF(PlanarGraph.dodecahedron)
  println(PlanarGraph.fromString("PlanarGraph(6,Vector(List(), List((3,6), (3,7), (5,6)), List((4,6), (4,8), (5,6))),ArrayBuffer((1,0), (1,0)),0)"))
  DrawPlanarGraph.showPDF(PlanarGraph.fromString("PlanarGraph(6,Vector(List(), List((3,6), (3,7), (5,6)), List((4,6), (4,8), (5,6))),ArrayBuffer((1,0), (1,0)),0)"))
}