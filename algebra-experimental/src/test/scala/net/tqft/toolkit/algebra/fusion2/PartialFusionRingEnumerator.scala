package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.mutable.ListBuffer

object PartialFusionRingEnumerator extends App {

  val defaultArgs = (4, 0, 120.0)
  
  val selfDualObjects = args.lift(0).map(_.toInt).getOrElse(defaultArgs._1)
  val dualPairs = args.lift(1).map(_.toInt).getOrElse(defaultArgs._2)
  val globalDimensionBound = args.lift(1).map(_.toDouble).getOrElse(defaultArgs._3)
  
  val enumeration = PartialFusionRingEnumeration(selfDualObjects, dualPairs, Some(globalDimensionBound))

  var counter = 0
  val dimensions = ListBuffer[Double]()
  
  val mod = 1024
  
  for(res <- (0 until mod).par; x <- enumeration.root.descendants(res = res, mod = mod); if x.remaining.isEmpty ) {
    println(x)
//    println(x.associativityToString)
    counter = counter + 1
    dimensions += x.globalDimensionLowerBound    
  }
  
  println(s"Saw $counter possibilities.")
  println("Global dimension lower bounds:")
  println(dimensions.toSeq.sorted)

}