package net.tqft.toolkit.algebra.spiders

object PlanarGraphEnumeration2App extends App {
  val dpg = DrawPlanarGraph.withOutputPath("/Users/emilypeters/Documents/scratch/graphs")
    val spider = implicitly[DiagramSpider[PlanarGraph]]

  //  {
  //  val context = PlanarGraphEnumerationContext2(Seq(VertexType(3, 0, 1)), Seq.tabulate(4)(i => PlanarGraph.polygon(i + 1)), maximumVertices = Some(11), maximumBoundaryPoints = Some(7))
  //  val root = PlanarGraph.star(3)
  //
  //  val descendants = context.descendants(root).toStream
  //
  //  val counts = for (b <- 3 to 7) yield {
  //    for (v <- 1 to 11) yield {
  //      descendants.count(g => g.numberOfBoundaryPoints == b && g.numberOfInternalVertices == v)
  //    }
  //  }
  //
  //  for (line <- counts) println(line)
  //  }

  {
    val mv = 5
    val mb= 4
    val root = PlanarGraph.star(4)
    val context = PlanarGraphEnumerationContext2(Seq(VertexType(4, 0, 1)), Seq(spider.multiply(root, root, 2)), maximumVertices = Some(mv), maximumBoundaryPoints = Some(mb))

    val descendants = context.descendants(root).toStream

    val counts = for (b <- 3 to mb) yield {
      for (v <- 1 to mv) yield {
        descendants.count(g => g.numberOfBoundaryPoints == b && g.numberOfInternalVertices == v)
      }
    }

    for (line <- counts) println(line)
  }

  //  println(descendants.filter(_.numberOfBoundaryPoints == 3).size)
  //  println(descendants.filter(_.numberOfBoundaryPoints == 4).size)
  //  println(descendants.filter(_.numberOfBoundaryPoints == 5).size)
  //  println(descendants.filter(_.numberOfBoundaryPoints == 6).size)

  //  for (
  //    c <- descendants if c.numberOfBoundaryPoints == 6
  //  ) {
  //    dpg.showPDF(c)
  //
  //  }
}