package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler

object PartialFusionRingEstimatorApp extends App {

  val L = 36.0
 
  for(e <- PartialFusionRing(2, Set(1), FusionRings.Examples.rank2(0), L).runtimeEstimators) {
    val t = e / 1000
    println(t / 86400 + " days " + (t % 86400) / 3600 + " hours " + (t % 3600) / 60 + " minutes " + t % 60 + " seconds")
  }
}