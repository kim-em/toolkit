package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler

object PartialFusionRingEstimatorApp extends App {

  val L = if(args.length > 0) {
    args(0).toDouble
  } else {
    12.0
  }
 
  for(e <- PartialFusionRing(1, FusionRings.Examples.rank1, L).runtimeEstimators) {
    val t = e / 1000
    println(t / 86400 + " days " + (t % 86400) / 3600 + " hours " + (t % 3600) / 60 + " minutes " + t % 60 + " seconds")
  }
}