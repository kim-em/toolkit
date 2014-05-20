package net.tqft.toolkit.algebra

package object spiders {
  implicit class RichSeq[A](x: Seq[A]) {
    def most = x.dropRight(1)
    def removed(i: Int) = x.take(i) ++ x.drop(i + 1)
    def secondLast = x(x.size - 2)
  }

  object TrivalentGraphs {
    val graphs = GraphsGeneratedBy(Seq((3, 1)))

    private val tinyFaces = for (i <- 1 to 3) yield PlanarGraph.polygon(i)
    private val smallFaces = for (i <- 1 to 4) yield PlanarGraph.polygon(i)

    val withoutTinyFaces = graphs.avoiding(tinyFaces)
    val withoutSmallFaces = graphs.avoiding(smallFaces)
  }
}