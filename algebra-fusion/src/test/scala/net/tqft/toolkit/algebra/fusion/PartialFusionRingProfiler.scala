package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler
import net.tqft.toolkit.collections.FlexibleTranspose._

object PartialFusionRingProfiler extends App {

  val enumeration = PartialFusionRingEnumeration(4, 0, Some(20.0))

  for(t <- Profiler.movingTimingAverages(3)(enumeration.root.descendants().size).take(5)) {
    println(t)
  }
}