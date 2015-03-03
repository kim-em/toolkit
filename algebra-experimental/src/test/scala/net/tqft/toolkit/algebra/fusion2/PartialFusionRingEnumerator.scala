package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.mutable.ListBuffer
import java.io.File
import scala.io.Source

object PartialFusionRingEnumerator extends App {

  val defaultArgs = (5, 0, 20.0)

  val selfDualObjects = args.lift(0).map(_.toInt).getOrElse(defaultArgs._1)
  val dualPairs = args.lift(1).map(_.toInt).getOrElse(defaultArgs._2)
  val globalDimensionBound = args.lift(1).map(_.toDouble).getOrElse(defaultArgs._3)

  val enumeration = PartialFusionRingEnumeration(selfDualObjects, dualPairs, Some(globalDimensionBound))

  val inputFile = new File(System.getProperty("user.home") + s"/projects/toolkit/fusion-rings/level1-$selfDualObjects-$dualPairs")
  if (!inputFile.exists) {
    println("Please run the LevelOne enumerator first.")
  }
  val lines = {
    import net.tqft.toolkit.collections.Iterators._
    require(Source.fromFile(inputFile).getLines.last == ".")
    Source.fromFile(inputFile).getLines
  }
  var finished = false
  val levelOne = {
    (for (line <- lines; if line != "."; dimensionString = line.split(" ").last; if dimensionString.toDouble < globalDimensionBound) yield {
      enumeration.PartialFusionRing(line)
    }).toStream
  }

  var counter = 0
  val dimensions = ListBuffer[Double]()

  val mod = 1024

  for (root <- levelOne) {
    println("Considering "+root)
    for(x <- root.descendants(); if x.remaining.isEmpty) {
    println(x)
    //    println(x.associativityToString)
    counter = counter + 1
    dimensions += x.globalDimensionLowerBound
  }
  }

  println(s"Saw $counter possibilities.")
  println("Global dimension lower bounds:")
  println(dimensions.toSeq.sorted)

}