package net.tqft.toolkit.algebra.fusion

object PartialFusionRingApp extends App {

  for(r <- PartialFusionRing(2, FusionRings.Examples.rank2(0), 8.0).descendants(); if r.depth == r.depths.max; if r.ring.associativityConstraints.forall(p => p._1 == p._2)) {
    println(r.depth)
    println(r.ring.structureCoefficients.map(_.entries))
  }
  
}