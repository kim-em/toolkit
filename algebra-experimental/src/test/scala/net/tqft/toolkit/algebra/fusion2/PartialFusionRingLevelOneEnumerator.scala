package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.Profiler
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import java.io.PrintWriter

object PartialFusionRingLevelOneEnumerator extends App {

  val defaultArgs = Seq(5, 0)

  val selfDualObjects = args.lift(0).map(_.toInt).getOrElse(defaultArgs(0))
  val dualPairs = args.lift(1).map(_.toInt).getOrElse(defaultArgs(1))

  val enumeration = PartialFusionRingEnumeration(selfDualObjects, dualPairs)

  var counter = 0

  val mod = 1024

  val outputFile = new File(System.getProperty("user.home") + s"/projects/toolkit/fusion-rings/level1-$selfDualObjects-$dualPairs")
  outputFile.delete
  val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(outputFile), "UTF-8"))

  val start = System.nanoTime
  def elapsed = (System.nanoTime - start) / 1000000000
  
  for (res <- (0 until mod).par; x <- enumeration.root.descendants({ r => 1 - r.level }, res, mod); if (x.level == 1)) {
    val ss = x.toShortString
    out.println(ss)
    println(ss)
    counter = counter + 1
  }

  out.println(".")
  out.flush
  out.close
  
  println(s"Saw $counter possibilities.")

}