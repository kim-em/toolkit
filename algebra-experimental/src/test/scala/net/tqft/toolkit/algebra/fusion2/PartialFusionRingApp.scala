package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler

object PartialFusionRingApp extends App {

  val enumeration = PartialFusionRingEnumeration(5, 0, 0.0)
  
  println(Profiler.timing(enumeration.root.boundedBreadthFirst(100)))
//  for(x <- enumeration.root.descendants()) {
//    println(x)
//  }
  
}