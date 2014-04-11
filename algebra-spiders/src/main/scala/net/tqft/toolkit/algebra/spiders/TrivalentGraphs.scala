package net.tqft.toolkit.algebra.spiders

object TrivalentGraphs {
  private val tinyFaces = for (i <- 1 to 3) yield PlanarGraph.polygon(i)
  private val smallFaces = for (i <- 1 to 4) yield PlanarGraph.polygon(i)

  def spider = implicitly[DiagramSpider[PlanarGraph]]

  def withoutTinyFacesAnd(faces: Seq[PlanarGraph]) = Enumerator(tinyFaces ++ faces)
  def withoutSmallFacesAnd(faces: Seq[PlanarGraph]) = Enumerator(smallFaces ++ faces)

  val withoutTinyFaces = Enumerator(tinyFaces)
  val withoutSmallFaces = Enumerator(smallFaces)

  case class Enumerator(avoidingFaces: Seq[PlanarGraph]) {

    def byNumberOfFaces(numberOfBoundaryPoints: Int, maximumNumberOfFaces: Int): Iterator[PlanarGraph] = {
      for (
        k <- (0 to numberOfBoundaryPoints + 2 * maximumNumberOfFaces - 2).iterator /* TODO is that right? */ ;
        g <- byNumberOfVertices(numberOfBoundaryPoints, numberOfBoundaryPoints /* FIXME that isn't right */ , k);
        if g.numberOfInternalFaces <= maximumNumberOfFaces
      ) yield g
    }

    val byNumberOfVertices = {
      import net.tqft.toolkit.functions.Memo._
      val cached = ({ t: (Int, Int, Int) => byNumberOfVertices_(t._1, t._2, t._3) }).memo;
      { (n: Int, g: Int, k: Int) => cached((n, g, k)) }
    }

    def byNumberOfVertices_(numberOfBoundaryPoints: Int, maximumGirth: Int, numberOfVertices: Int): Stream[PlanarGraph] = {
      def addHs(graph: PlanarGraph) = for (j <- 0 until graph.numberOfBoundaryPoints) yield spider.rotate(spider.multiply(spider.rotate(graph, j), PlanarGraph.H, 2), -j)
      def addForks(graph: PlanarGraph) = for (j <- 0 until graph.numberOfBoundaryPoints + 1) yield spider.rotate(spider.multiply(spider.rotate(graph, Seq(graph.numberOfBoundaryPoints - 1, j).max), spider.rotate(PlanarGraph.star(3), -2), 1), -j)
      def addCups(graph: PlanarGraph) = for (j <- 0 until graph.numberOfBoundaryPoints + 2) yield spider.rotate(spider.tensor(spider.rotate(graph, Seq(graph.numberOfBoundaryPoints - 1, j).max), PlanarGraph.strand), -j)
      //    def addCaps(graph: PlanarGraph) = for (j <- 0 until graph.numberOfBoundaryPoints) yield spider.rotate(spider.multiply(spider.rotate(graph, j), PlanarGraph.strand, 2), -Seq(graph.numberOfBoundaryPoints - 2, j).max)

      // FIXME add caps sometimes!

      if (numberOfBoundaryPoints < 0 || numberOfVertices < 0) {
        Stream.empty
      } else {
        if (numberOfBoundaryPoints == 0 && numberOfVertices == 0) {
          Stream(PlanarGraph.empty)
        } else {
          val candidates = (byNumberOfVertices(numberOfBoundaryPoints, maximumGirth, numberOfVertices - 2).flatMap(addHs) ++
            byNumberOfVertices(numberOfBoundaryPoints - 1, maximumGirth, numberOfVertices - 1).flatMap(addForks) ++
            byNumberOfVertices(numberOfBoundaryPoints - 2, maximumGirth, numberOfVertices).flatMap(addCups))

          val distinct = candidates.map(spider.canonicalFormWithDefect)
            .map(_._1)
            .distinct

          distinct.filter(g => g.loops == 0 && avoidingFaces.forall(f => g.Subgraphs(f).excisions.isEmpty))
        }
      }

    }
  }

}
