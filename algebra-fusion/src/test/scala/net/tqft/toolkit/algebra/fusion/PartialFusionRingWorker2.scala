package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.mutable.ListBuffer
import java.io.File
import scala.io.Source
import net.tqft.toolkit.algebra.enumeration.TreeReader
import java.io.PrintWriter
import java.io.FileOutputStream
import net.tqft.toolkit.algebra.enumeration.TreePrinter
import scala.io.StdIn
import scala.concurrent.Future
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.nio.file.Files

object PartialFusionRingWorker2 extends App {

  case class Config(
    globalDimensionBound: Double = 12.0,
    levelBound: Option[Int] = None,
    stepsBound: Option[Int] = None,
    res: Int = 0, mod: Int = 1,
    finishBy: Option[Long] = None,
    cpus: Option[Int] = None,
    batch: Boolean = false)

  val parser = new scopt.OptionParser[Config]("PartialFusionRingWorker2") {
    head("PartialFusionRingWorker", "1.0")
    help("help") text ("prints this usage text")
    opt[Int]('l', "level") valueName ("<level>") action { (x, c) =>
      c.copy(levelBound = Some(x))
    } text ("only enumerate fusion rings up to level <level>")
    opt[Int]('s', "step") valueName ("<step>") action { (x, c) =>
      c.copy(stepsBound = Some(x))
    } text ("only enumerate fusion rings up to step <step> (steps including fixing entries and increasing the level)")
    opt[Double]('h', "hours") valueName ("<hours>") action { (x, c) =>
      c.copy(finishBy = Some(System.currentTimeMillis() + (x * 60 * 60 * 1000).toLong))
    } text ("run for at most <hours> hours")
    opt[Double]('c', "cpu-factor") valueName ("<factor>") action { (x, c) =>
      c.copy(cpus = Some((Runtime.getRuntime.availableProcessors() * x).toInt))
    } text ("run with <factor>*#cpus threads")
    opt[Unit]('q', "batch") action { (_, c) =>
      c.copy(batch = true)
    } text ("disable keyboard interrupt")
    opt[Seq[Int]]('r', "resmod") valueName ("<res>,<mod>") action {
      case (Seq(r, m), c) =>
        c.copy(res = r, mod = m)
    } text ("select only work in residue class res/mod")
    arg[Double]("<global-dimension-bound>") hidden () action { (x, c) => c.copy(globalDimensionBound = x) }
  }

  parser.parse(args, Config()) map { config =>

    val dir = new File("fusion-rings2")
    if (!dir.exists) {
      dir.mkdir()
    }

    var pleaseFinishNow = false
    new File("please-stop").delete

    def accept(r: PartialFusionRingWithInvertiblesEnumeration#PartialFusionRing): Int = {
      val checks = Seq(
        r.globalDimensionLowerBound <= config.globalDimensionBound,
        config.levelBound.isEmpty || r.level < config.levelBound.get,
        config.stepsBound.isEmpty || r.steps < config.stepsBound.get,
        config.finishBy.isEmpty || System.currentTimeMillis < config.finishBy.get,
        !pleaseFinishNow,
        !(new File("please-stop").exists))

      if (checks.forall(_ == true)) { 1 } else { 0 }
    }

    def targets(enumeration: PartialFusionRingWithInvertiblesEnumeration): Iterator[enumeration.PartialFusionRing] = {
      val initialString = enumeration.root.toShortString.split(" ").take(3).mkString(" ")

      val leafIterator = if (Files.newDirectoryStream((new File("fusion-rings2")).toPath, initialString + "*.tree").iterator.hasNext) {
        TreeReader.readLeaves(new File("fusion-rings2"), initialString)
      } else {
        Iterator(enumeration.root.toShortString)
      }
      leafIterator 
        .filter(l => !pleaseFinishNow)
        .map(l => (l, l.split(" ")))
        .filter(_._2.size == 6)
        .filter(config.levelBound.isEmpty || _._2(3).toInt <= config.levelBound.get)
        .filter(_._2(5).toDouble <= config.globalDimensionBound)
        .map(_._1)
        .map(s => enumeration.PartialFusionRing(s))
        .filter(r => accept(r) > 0)
        .filter(r => config.mod == 1 || r.hashCode.abs % config.mod == config.res)
    }

    def allTargets = for (
      orbitStructure <- OrbitStructures(config.globalDimensionBound);
      dualData <- orbitStructure.compatibleDualData;
      enumeration = PartialFusionRingWithInvertiblesEnumeration(orbitStructure, dualData, Some(config.globalDimensionBound));
      if enumeration.rootOption.nonEmpty;
      t <- targets(enumeration)
    ) yield t

    import net.tqft.toolkit.collections.Iterators._

    var counter = 0
    val total = 0 // allTargets.size

    def verboseTargets = allTargets.map({ x =>
      {
        counter = counter + 1
        println(s"Found target ($counter/$total) ${x.toShortString}")
        x
      }
    })

    if (!config.batch) {
      import scala.concurrent.ExecutionContext.Implicits.global
      Future {
        println("Hit <enter> to request that everyone finishes up quickly and goes home.")
        StdIn.readLine
        println("Cleaning up ...")
        pleaseFinishNow = true
      }
    }

    import net.tqft.toolkit.collections.ParIterator._

    for (t <- verboseTargets) {
//    for (t <- config.cpus.map(c => verboseTargets.parWithNumberOfThreads(c)).getOrElse(verboseTargets.par)) {
      TreePrinter[PartialFusionRingWithInvertiblesEnumeration#PartialFusionRing](_.toShortString, _.steps, accept)
        .to("fusion-rings2", t)
        .print(t.descendants(accept))
      println("Finished target " + t.toShortString)
    }

    for (t <- config.finishBy) {
      if (System.currentTimeMillis >= t) println("Time limited exceeded.")
    }

    System.exit(0)
  } getOrElse {
    // arguments are bad, usage message will have been displayed
  }

}