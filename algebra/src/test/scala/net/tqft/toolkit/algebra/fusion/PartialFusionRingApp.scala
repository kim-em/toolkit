package net.tqft.toolkit.algebra.fusion

object PartialFusionRingApp extends App {

  for(r <- PartialFusionRing(2, FusionRings.Examples.rank2(0), 12.0).descendants(); if r.depth == r.depths.max) {
     if(r.ring.associativityConstraints.forall(p => p._1 == p._2)) {
       println("Found a depth " + r.depth + " fusion ring: " + r.ring.structureCoefficients.map(_.entries))
     } else {
       println("   and an incomplete one with global dimension " + r.ring.globalDimensionLowerBound + ": " + r.ring.structureCoefficients.map(_.entries))
     }
  }
  
}