package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler

object FusionRingsProfiler extends App {

  while(true) {
    for(t <- Profiler.movingTimingAverages(3)(FusionRings.withObject(FusionRings.Examples.AH1.structureCoefficients(1)))) {
      println(t)
    }
  }
    
  
}