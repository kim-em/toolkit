package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.Profiler

object GraphsGeneratedByProfiler extends App {
  def trivalentEnumerator = GraphsGeneratedBy(Seq((3, 1)))
  def withoutSmallFaces = trivalentEnumerator.avoiding(for (i <- 1 to 4) yield PlanarGraph.polygon(i))

  import Profiler._
  for(t <- successiveTimingAverages({
    withoutSmallFaces.byNumberOfFaces(9, 1).size
    withoutSmallFaces.byNumberOfFaces(10, 2).size
    withoutSmallFaces.byNumberOfFaces(11, 3).size
    withoutSmallFaces.byNumberOfFaces(12, 4).size
  })) println(t)
}