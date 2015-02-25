package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.mutable.ListBuffer

object PartialFusionRingApp extends App {

  val enumeration = PartialFusionRingEnumeration(4, 1)

  var counter = 0
  val dimensions = ListBuffer[Double]()
  
  for(x <- enumeration.root.parDescendants({ r => 1 - r.level }); if (x.level == 1)) {
    println(x)
    println(x.associativityToString)
    counter = counter + 1
    dimensions += x.globalDimensionLowerBound    
  }
  
  println(s"Saw $counter possibilities.")
  println("Global dimension lower bounds:")
  println(dimensions.toSeq.sorted)

}