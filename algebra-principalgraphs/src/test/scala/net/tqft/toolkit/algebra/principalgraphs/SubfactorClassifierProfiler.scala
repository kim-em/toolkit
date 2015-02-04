package net.tqft.toolkit.algebra.principalgraphs

import SubfactorClassifier._
import net.tqft.toolkit.Profiler

object SubfactorClassifierProfiler extends App {

  for(t <- Profiler.movingTimingAverages(3)(require(A3(5.0).descendantsTreeFiltered(3, -1, 100, ignoring).size == 873))) {
    println(t)
  }

}