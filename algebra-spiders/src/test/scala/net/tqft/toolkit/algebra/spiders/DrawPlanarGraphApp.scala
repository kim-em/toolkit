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

  //  DrawPlanarGraph.showPDF(PlanarGraph(16,Vector(List((18,16), (19,28), (7,17), (8,18), (9,19), (10,20)), Vector((11,21), (6,17), (15,16)), Vector((12,21), (7,18), (11,17)), Vector((13,21), (8,19), (12,18)), Vector((14,21), (9,20), (13,19)), Vector((15,21), (10,16), (14,20)), List((6,16), (19,17), (18,28))),Vector((1,0), (1,0), (1,0), (1,0), (1,0), (1,1)),0))
  //  DrawPlanarGraph.showPDF(PlanarGraph.spider.tensor(PlanarGraph.spider.tensor(PlanarGraph.polygon(5), PlanarGraph.I), PlanarGraph.crossing))

  //  DrawPlanarGraph.showPDF(PlanarGraph.spider.rotate(PlanarGraph.spider.tensor(PlanarGraph.polygon(5), PlanarGraph.I),0))
  //  DrawPlanarGraph.showPDF(PlanarGraph.spider.rotate(PlanarGraph.spider.tensor(PlanarGraph.polygon(5), PlanarGraph.I),1))
  //  DrawPlanarGraph.showPDF(PlanarGraph.spider.rotate(PlanarGraph.spider.tensor(PlanarGraph.polygon(5), PlanarGraph.I),2))

  DrawPlanarGraph.showPDF(PlanarGraph(22,Vector(List(), List((11,32), (19,28), (16,30), (15,24), (14,27)), List((15,27), (16,24), (21,30), (18,29), (17,26)),List((12,28), (13,23), (20,31), (21,29), (19,30)), List((7,22), (10,25), (18,26), (20,29), (8,31)), List((7,25), (9,22), (14,32), (17,27), (10,26)), List((8,22), (13,31), (12,23), (11,28), (9,32))),List((1,0), (1,0), (1,0), (1,0), (1,0), (1,0)),0,None))
  
  val dpg = DrawPlanarGraph.withOutputPath("/Users/scott/projects/exceptional/diagrams/graphs/")

  val cuboctahedron = PlanarGraph.fromString("PlanarGraph(35,Vector(List(), List((5,6), (4,9), (3,8), (2,7)), List((2,6), (10,7), (9,18), (8,17)), List((8,6), (15,17), (14,27), (13,26)), List((10,18), (3,7), (19,8), (18,35)), List((18,18), (26,35), (25,46), (24,45)), List((24,18), (31,45), (15,27), (9,17)), List((26,46), (19,35), (35,8), (34,54)), List((34,46), (42,54), (41,65), (40,64)), List((40,46), (47,64), (31,27), (25,45)), List((42,65), (35,54), (4,8), (50,9)), List((50,65), (5,9), (13,6), (56,26)), List((56,65), (14,26), (47,27), (41,64))),Vector((2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0)),0)")
  val polyhedron12G = PlanarGraph.fromString("PlanarGraph(46,Vector(List(), List((5,6), (4,9), (3,8), (2,7)), List((2,6), (10,7), (9,18), (8,17)), List((8,6), (15,17), (14,27), (13,26)), List((13,6), (20,26), (19,36), (5,9)), List((26,36), (25,46), (24,45), (19,9)), List((20,36), (14,26), (30,27), (26,46)), List((10,18), (3,7), (34,8), (33,54)), List((4,8), (24,9), (40,45), (34,54)), List((25,45), (47,46), (46,75), (40,54)), List((54,75), (53,86), (33,18), (46,54)), List((15,27), (9,17), (53,18), (57,86)), List((47,75), (30,46), (57,27), (54,86))),Vector((2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0)),0)")
  println(dpg.filenameForGraph(cuboctahedron))
  dpg.createPDF(cuboctahedron)
  println(dpg.filenameForGraph(polyhedron12G))
  dpg.createPDF(polyhedron12G)

}