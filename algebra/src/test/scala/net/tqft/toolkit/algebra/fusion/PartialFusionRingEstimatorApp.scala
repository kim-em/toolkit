package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler

object PartialFusionRingEstimatorApp extends App {

  val L = 36.0
 
  for(e <- PartialFusionRing(2, Seq(1), FusionRings.Examples.rank2(4), L).runtimeEstimators) {
    val t = e / 1000
    println(t / 86400 + " days " + (t % 86400) / 3600 + " hours " + (t % 3600) / 60 + " minutes " + t % 60 + " seconds")
  }
}