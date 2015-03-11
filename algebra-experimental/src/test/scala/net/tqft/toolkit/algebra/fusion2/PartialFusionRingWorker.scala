package net.tqft.toolkit.algebra.fusion2

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

object PartialFusionRingWorker extends App {

  case class Config(selfDualObjects: Int = 5, dualPairs: Int = 0, res: Int = 0, mod: Int = 1, globalDimensionBound: Option[Double] = None, levelBound: Option[Int] = None, stepsBound: Option[Int] = None, finishBy: Option[Long] = None, cpus: Option[Int] = None, batch: Boolean = false, verbose: Boolean = false)

  val parser = new scopt.OptionParser[Config]("PartialFusionRingWorker") {
    head("PartialFusionRingWorker", "1.0")
    opt[Double]('D', "global-dimensions") action { (x, c) =>
      c.copy(globalDimensionBound = Some(x))
    } text ("only enumerate fusion rings with bounded global dimension")
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
    //    opt[Unit]("verbose") action { (_, c) =>
    //      c.copy(verbose = true)
    //    } text ("verbose is a flag")
    help("help") text ("prints this usage text")
    arg[Int]("<number-of-self-dual-objects>") hidden () action { (x, c) => c.copy(selfDualObjects = x) }
    arg[Int]("<number-of-dual-pairs>") hidden () action { (x, c) => c.copy(dualPairs = x) }
    checkConfig { c =>
      if (c.globalDimensionBound.nonEmpty || c.levelBound.nonEmpty || c.stepsBound.nonEmpty || c.finishBy.nonEmpty) success else failure("You have requested an infinite computation; please specify at least one bound.")
    }
  }

  parser.parse(args, Config()) map { config =>
    val enumeration = PartialFusionRingEnumeration(config.selfDualObjects, config.dualPairs)

    val initialString = config.selfDualObjects + "," + config.dualPairs

    val dir = new File("fusion-rings")
    if (!dir.exists) {
      dir.mkdir()
    }

    val seedFile = new File("fusion-rings/" + enumeration.root.toShortString + ".tree")
    if (!seedFile.exists) {
      println(s"It seems no work has been done yet for $initialString; creating a seed file.")
      val pw = new PrintWriter(new FileOutputStream(seedFile))
      pw.println(enumeration.root.toShortString)
      pw.close
    }

    var pleaseFinishNow = false

    def accept(r: enumeration.PartialFusionRing): Int = {
      val checks = Seq(
        config.globalDimensionBound.isEmpty || r.globalDimensionLowerBound <= config.globalDimensionBound.get,
        config.levelBound.isEmpty || r.level < config.levelBound.get,
        r.level < 9,
        config.stepsBound.isEmpty || r.steps < config.stepsBound.get,
        config.finishBy.isEmpty || System.currentTimeMillis < config.finishBy.get,
        !pleaseFinishNow,
        !(new File("please-stop").exists))

      if (checks.forall(_ == true)) { 1 } else { 0 }
    }

    val sourceFile = if (config.mod > 1) {
      seedFile
    } else {
      new File("fusion-rings")
    }

    import net.tqft.toolkit.collections.Iterators._
    val targets = TreeReader
      .readLeaves(sourceFile, initialString)
      .map(l => (l, l.split(" ")))
      .filter(_._2.size == 4)
      .filter(config.levelBound.isEmpty || _._2(1).toInt <= config.levelBound.get)
      .filter(config.globalDimensionBound.isEmpty || _._2(3).toDouble <= config.globalDimensionBound.get)
      .map(_._1)
      .map(enumeration.PartialFusionRing.apply)
      .filter(r => accept(r) > 0)
      .drop(config.res)
      .takeEvery(config.mod)
      .map({ x => println("Found target " + x.toShortString); x })

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

    for (t <- config.cpus.map(c => targets.parWithNumberOfThreads(c)).getOrElse(targets.par)) {
      TreePrinter[enumeration.PartialFusionRing](_.toShortString, _.steps, accept)
        .to("fusion-rings", t)
        .print(t.descendants(accept))
      println("Finished target " + t.toShortString)
    }

    for (t <- config.finishBy) {
      if (System.currentTimeMillis >= t) println("Time limited exceeded.")
    }
  } getOrElse {
    // arguments are bad, usage message will have been displayed
  }

}