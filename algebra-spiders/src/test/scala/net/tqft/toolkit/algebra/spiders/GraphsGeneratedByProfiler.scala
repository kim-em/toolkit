package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.Profiler

object GraphsGeneratedByProfiler extends App {
  import Profiler._
  for (
    t <- successiveTimingAverages({
      TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(9, 0).size
      TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(9, 1).size
      TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(10, 0).size
      TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(11, 0).size
    })
  ) println(t)
}