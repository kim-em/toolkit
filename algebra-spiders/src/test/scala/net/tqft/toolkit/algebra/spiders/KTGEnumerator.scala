package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.spiders.examples.QuantumExceptionalSeries

object KTGEnumerator extends App {
  val KTGEnumerator = GraphsGeneratedBy(Seq((3, 0, 1), (4, 0, 2)))
  val freeEnumerator = KTGEnumerator.avoiding(Seq(PlanarGraph.polygon(1)))
  val reducedEnumerator = KTGEnumerator.avoiding(QuantumExceptionalSeries.reductions.map(_.big))

  for (pg <- freeEnumerator.byNumberOfVertices(0, Map(VertexType(3, 0, 1) -> 4, VertexType(4, 0, 2) -> 5))) {
    println(pg)
  }
  for (pg <- freeEnumerator.byNumberOfFaces(0, 4)) {
    println(pg)
  }
}