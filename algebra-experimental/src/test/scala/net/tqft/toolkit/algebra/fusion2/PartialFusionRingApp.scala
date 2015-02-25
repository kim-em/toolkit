package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.mutable.ListBuffer

object PartialFusionRingApp extends App {

  val enumeration = PartialFusionRingEnumeration(4, 1)

  val pool = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(20))
  val mod = 120
  val range = (0 until mod).par
  range.tasksupport = pool

  var counter = 0
  val dimensions = ListBuffer[Double]()
  for (res <- range; x <- enumeration.root.descendants({ r => 1 - r.level }, res, mod); if(x.level == 1)) {
    println(x)
    println(x.associativity)
    counter = counter + 1
    dimensions += x.globalDimensionLowerBound
  }
  
  println(s"Saw $counter possibilities.")
  println("Global dimension lower bounds:")
  println(dimensions.toSeq.sorted)

}