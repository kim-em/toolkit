package net.tqft.toolkit.algebra.principalgraphs

import SubfactorClassifier._
import net.tqft.toolkit.Profiler

object SubfactorClassifierProfiler extends App {

//  SubfactorWeed.experimental = true
  for(t <- Profiler.movingTimingAverages(3)(require(A3(5.01).descendantsTreeFiltered(3, -1, -1, ignoring).size == 1054))) {
    println(t)
  }

}