package net.tqft.toolkit.algebra.spiders

object PlanarGraphEnumerationApp extends App {
  val dpg = {
    System.getProperty("user.name") match {
      case "scott"       => DrawPlanarGraph.withOutputPath("/Users/scott/scratch/graphs")
      case "emilypeters" => DrawPlanarGraph.withOutputPath("/Users/emilypeters/Documents/scratch/graphs")
    }
  }

  val spider = implicitly[DiagramSpider[PlanarGraph]]

  {
    // two trivalent vertices, no relations
    val context = PlanarGraphEnumerationContext(Seq(VertexType(3, 0, 1), VertexType(3, 1, 1)), Seq.empty, Seq(PlanarGraph.star(3, 0, 1), PlanarGraph.star(3, 1, 1)), 4, 1)
    println(context.maximumVertices)
    val p = context.parent(PlanarGraph.polygon(4))
    dpg.showPDF(p)
    assert(context.verify_child_of_parent(PlanarGraph.polygon(4)))

    //      for(a <- context.ancestry(PlanarGraph.polygon(4))) {
    //        dpg.showPDF(a)
    //        println(context.descendants(a).contains(PlanarGraph.polygon(4)))
    //      }
    //
    //
  }

}
