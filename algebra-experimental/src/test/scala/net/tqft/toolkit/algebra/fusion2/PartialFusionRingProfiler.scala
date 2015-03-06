package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler
import net.tqft.toolkit.collections.FlexibleTranspose._

object PartialFusionRingProfiler extends App {

  val enumeration = PartialFusionRingEnumeration(6, 0, Some(20.0))

  for(t <- Profiler.movingTimingAverages(3)(enumeration.root.dyadicDescendants(res = 0, exponent = 2).size).take(5)) {
    println(t)
  }
}