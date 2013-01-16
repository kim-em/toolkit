package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler

object PartialFusionRingApp extends App {

  while (true) {
    println("completed in " + Profiler.timing(
      for (r <- PartialFusionRing(2, FusionRings.Examples.rank2(0), 9.0).descendants(); if r.depth == r.depths.max) {
        if (r.ring.associativityConstraints.forall(p => p._1 == p._2)) {
          println("Found a depth " + r.depth + " fusion ring: " + r.ring.structureCoefficients.map(_.entries))
        } else {
          //       println("   and an incomplete one with global dimension " + r.ring.globalDimensionLowerBound + ": " + r.ring.structureCoefficients.map(_.entries))
        }
      })._1)
  }

  // 2013-01-15
  // PartialFusionRing(2, FusionRings.Examples.rank2(0), 9.0).descendants()
  // 27000ms
  // 13100ms BoundedDiophantineSolver doesn't optimizing choosing a branch when there are no equations left
  // 12100ms being a bit lazier finding FP eigenvalues
  // 8900ms (switching to times after warmup)
  // 8400ms trimming partialAssociativityConstraints
  // 7300ms trimming dualityConstraints
  // 6100ms optimizing constraints
}