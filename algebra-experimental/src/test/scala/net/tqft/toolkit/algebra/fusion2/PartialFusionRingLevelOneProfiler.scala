package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import java.io.PrintWriter

object PartialFusionRingLevelOneProfiler extends App {

  val defaultArgs = Seq(6, 0)

  val selfDualObjects = args.lift(0).map(_.toInt).getOrElse(defaultArgs(0))
  val dualPairs = args.lift(1).map(_.toInt).getOrElse(defaultArgs(1))

  val enumeration = PartialFusionRingEnumeration(selfDualObjects, dualPairs)

  val mod = 1024*10

  for (res <- 0 until mod) {
    println(Profiler.timing(enumeration.root.descendants({ r => 1 - r.level }, res, mod).size))
  }

}