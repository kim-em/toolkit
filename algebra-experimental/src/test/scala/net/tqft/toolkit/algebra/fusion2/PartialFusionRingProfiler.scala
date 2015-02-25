package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler
import net.tqft.toolkit.collections.FlexibleTranspose._

object PartialFusionRingProfiler extends App {

  val min = 2
  val enumerations = Range(min, 6).map(k => PartialFusionRingEnumeration(k, 1, Some(0.0)))
  val estimators = enumerations.map(_.root.runtimeEstimatorStrings)

  while (true) {
    for ((estimator,k) <- estimators.zipWithIndex) {
      println((k + min) + " + 2*1: " + estimator.next)
    }
    println("-----")
  }
}