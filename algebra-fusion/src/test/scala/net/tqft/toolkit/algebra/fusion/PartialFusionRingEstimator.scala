package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler
import net.tqft.toolkit.collections.FlexibleTranspose._

object PartialFusionRingEstimator extends App {

  val pairs = Seq((2,2), (4,1), (6,0))
  
  val enumerations = pairs.map(p => PartialFusionRingEnumeration(p._1, p._2, Some(0.0)))
  val estimators = enumerations.map(_.root.runtimeEstimatorStrings)

  while (true) {
    for ((estimator,p) <- estimators.zip(pairs)) {
      println(p + ": " + estimator.next)
    }
    println("-----")
  }
}