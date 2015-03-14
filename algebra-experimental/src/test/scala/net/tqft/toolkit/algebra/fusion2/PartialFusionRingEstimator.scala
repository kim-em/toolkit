package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler
import net.tqft.toolkit.collections.FlexibleTranspose._

object PartialFusionRingEstimator extends App {

  val min = 2
  
  val pairs = Seq((4,0),(5,0),(6,0),(7,0),(2,1),(3,1),(4,1),(5,1),(1,2),(2,2),(3,2),(1,3))
  
  val enumerations = pairs.map(p => PartialFusionRingEnumeration(p._1, p._2, Some(0.0)))
  val estimators = enumerations.map(_.root.runtimeEstimatorStrings)

  while (true) {
    for ((estimator,p) <- estimators.zip(pairs)) {
      println(p + ": " + estimator.next)
    }
    println("-----")
  }
}