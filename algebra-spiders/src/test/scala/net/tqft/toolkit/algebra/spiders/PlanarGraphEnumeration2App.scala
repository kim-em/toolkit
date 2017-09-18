package net.tqft.toolkit.algebra.spiders

object PlanarGraphEnumeration2App extends App {
  val dpg = DrawPlanarGraph.withOutputPath("/Users/scott/scratch/graphs")

  val context = PlanarGraphEnumerationContext2(Seq(VertexType(3, 0, 1)), Seq.tabulate(4)(i => PlanarGraph.polygon(i + 1)), maximumVertices = Some(8), maximumBoundaryPoints = Some(6))
  val root = PlanarGraph.star(3)

  val descendants = context.descendants(root).toStream

  val counts = for (b <- 3 to 6) yield {
    for (v <- 1 to 8) yield {
      descendants.count(g => g.numberOfBoundaryPoints == b && g.numberOfInternalVertices == v)
    }
  }

  for (line <- counts) println(line)

  for (c <- descendants.filter(g => g.numberOfBoundaryPoints == 6 && g.numberOfInternalVertices == 8)) dpg.showPDF(c)

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