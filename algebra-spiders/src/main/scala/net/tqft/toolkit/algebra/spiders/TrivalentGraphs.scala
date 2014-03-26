package net.tqft.toolkit.algebra.spiders

object TrivalentGraphs {
  private val smallFaces = for (i <- 1 to 4) yield PlanarGraph.polygon(1)

  def spider = implicitly[DiagramSpider[PlanarGraph]]

  def withoutSmallFaces = without(smallFaces) _
  def withoutSmallFacesAnd(faces: Seq[PlanarGraph]) = without(smallFaces ++ faces) _

  def without(faces: Seq[PlanarGraph])(n: Int, g: Int, k: Int): Seq[PlanarGraph] = {
    // TODO double check these rotations are correct!
    def addHs(graph: PlanarGraph) = for (j <- 0 until graph.numberOfBoundaryPoints) yield spider.rotate(spider.multiply(spider.rotate(graph, j), PlanarGraph.H, 2), -j)
    def addForks(graph: PlanarGraph) = for (j <- 0 until graph.numberOfBoundaryPoints + 1) yield spider.rotate(spider.multiply(spider.rotate(graph, Seq(graph.numberOfBoundaryPoints - 1, j).max), spider.rotate(PlanarGraph.star(3), -2), 1), -j)
    def addCups(graph: PlanarGraph) = for (j <- 0 until graph.numberOfBoundaryPoints + 2) yield spider.rotate(spider.tensor(spider.rotate(graph, Seq(graph.numberOfBoundaryPoints - 1, j).max), PlanarGraph.strand), -j)
    def addCaps(graph: PlanarGraph) = for (j <- 0 until graph.numberOfBoundaryPoints) yield spider.rotate(spider.multiply(spider.rotate(graph, j), PlanarGraph.strand, 2), -Seq(graph.numberOfBoundaryPoints - 2, j).max)

    if (n < 0 || k < 0 || n > g) {
      Seq.empty
    } else {
      if (n == 0 && k == 0) {
        Seq(PlanarGraph.empty)
      } else {
        (without(faces)(n, g, k - 2).flatMap(addHs) ++
          without(faces)(n - 1, g, k - 1).flatMap(addForks) ++
          without(faces)(n - 2, g, k).flatMap(addCups) ++
          without(faces)(n + 2, g, k).flatMap(addCaps))
          .map(spider.canonicalFormWithDefect)
          .map(_._1)
          .distinct
          .filter(g => g.loops == 0 && faces.forall(f => g.Subgraphs(f).excisions.isEmpty))
      }
    }
  }
}
