package net.tqft.toolkit.algebra.spiders

object PlanarGraphEnumeration2App extends App {
  val dpg = {
    System.getProperty("user.name") match {
      case "scott"       => DrawPlanarGraph.withOutputPath("/Users/scott/scratch/graphs")
      case "emilypeters" => DrawPlanarGraph.withOutputPath("/Users/emilypeters/Documents/scratch/graphs")
    }
  }

  val spider = implicitly[DiagramSpider[PlanarGraph]]

  {
    val maxVertices = 6 // we've tested up to 8
    val maxBoundaryPoints = 10 // we've tested up to 10
    val root = PlanarGraph.star(4)
    val bigon = spider.multiply(root, root, 2)
    val triangle = spider.multiply(spider.rotate(spider.multiply(root, root, 1), 2), root, 2)
    val context = PlanarGraphEnumerationContext2(Seq(VertexType(4, 0, 1)), Seq(bigon, triangle), maximumVertices = Some(maxVertices), maximumBoundaryPoints = Some(maxBoundaryPoints))

    val descendants = context.descendants(root).toStream

    val counts = for (b <- 3 to maxBoundaryPoints) yield {
      for (v <- 1 to maxVertices) yield {
        descendants.count(g => g.numberOfBoundaryPoints == b && g.numberOfInternalVertices == v)
      }
    }

    for (line <- counts) println(line)
  }

}