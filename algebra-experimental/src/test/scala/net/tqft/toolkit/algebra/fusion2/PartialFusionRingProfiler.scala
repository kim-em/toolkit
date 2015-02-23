package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler

object PartialFusionRingProfiler extends App {

  val enumeration = PartialFusionRingEnumeration(5, 0, 60.0)
  
  
  for(x <- enumeration.root.runtimeEstimators) {
    println(x)
  }
  
}