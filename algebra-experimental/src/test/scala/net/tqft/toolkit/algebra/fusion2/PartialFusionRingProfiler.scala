package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler
import net.tqft.toolkit.collections.FlexibleTranspose._

object PartialFusionRingProfiler extends App {
  
  val enumerations = Range(2,12).map(k => PartialFusionRingEnumeration(k, 0, 0.0))
  val estimators = enumerations.map(_.root.runtimeEstimatorStrings)
  
  while(true) {
  for(estimator <- estimators) {
    println(estimator.next)
  }
  }
}