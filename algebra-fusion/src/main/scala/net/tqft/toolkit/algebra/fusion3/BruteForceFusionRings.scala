package net.tqft.toolkit.algebra.fusion3

import net.tqft.toolkit.Profiler
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.StdIn
import java.io.File
import scala.io.Source
import java.io.PrintWriter
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.BufferedWriter

object BruteForceFusionRings extends App {

  case class Config(
    selfDualObjects: Int = 5,
    dualPairs: Int = 0,
    globalDimensionBound: Double = 60.0,
    resumable: Boolean = false,
    finishBy: Option[Long] = None,
    batch: Boolean = false)

  val parser = new scopt.OptionParser[Config]("BruteForceFusionRings") {
    head("BruteForceFusionRings", "1.0")
    opt[Unit]('r', "resumable") action { (_, c) =>
      c.copy(resumable = true)
    } text ("resume existing work, if available, or checkpoint resumable data on <enter>")
    opt[Double]('h', "hours") valueName ("<hours>") action { (x, c) =>
      c.copy(finishBy = Some(System.currentTimeMillis() + (x * 60 * 60 * 1000).toLong))
    } text ("run for at most <hours> hours")
    opt[Unit]('q', "batch") action { (_, c) =>
      c.copy(batch = true)
    } text ("disable keyboard interrupt")
    help("help") text ("prints this usage text")
    arg[Int]("<number-of-self-dual-objects>") hidden () action { (x, c) => c.copy(selfDualObjects = x) }
    arg[Int]("<number-of-dual-pairs>") hidden () action { (x, c) => c.copy(dualPairs = x) }
    arg[Double]("<global-dimension-bound>") hidden () action { (x, c) => c.copy(globalDimensionBound = x) }
  }

  parser.parse(args, Config()) map { config =>
    val enumeration = Enumeration(config.selfDualObjects, config.dualPairs, config.globalDimensionBound)

    new File("fusion-rings3/").mkdir
    val prefix = "fusion-rings3/" + config.selfDualObjects + "," + config.dualPairs + "," + config.globalDimensionBound
    val inFile = new File(prefix + ".in")
    val outFile = new File(prefix + ".out")

    val notify: enumeration.Complete => Unit = {
      if (config.resumable) {
        // lazy, to avoid creating the file until it's needed
        lazy val pw = new PrintWriter(new BufferedWriter(new FileWriter(outFile, true)));
        { c =>
          println(c)
          pw.println(c)
          pw.flush
        }
      } else {
        println _
      }
    }

    val (time, (toResume, numberFound)) = (Profiler.timing({
      val targets: Seq[enumeration.Partial] = if (config.resumable && inFile.exists) {
        Source.fromFile(inFile).getLines.map(enumeration.Partial.apply).toSeq
      } else if(config.resumable && outFile.exists) {
        Seq.empty
      } else {
        Seq(enumeration.root)
      }

      println("--- starting enumeration, from the following targets:")
      for (t <- targets) println(t)
      println("---")

      val (futures, interrupts) = {
        val s = targets.map(_.interruptibleDescendants(notify))
        (s.map(_._1), s.map(_._2))
      }

      val interrupt = { () => for (i <- interrupts) i() }
      
      val future = {        
        import scala.concurrent.ExecutionContext.Implicits.global
        Future {
          val results = futures.map(f => Await.result(f, Duration.Inf))
          (results.flatMap(_._1), results.flatMap(_._2))
        }
      }
      
      if (!config.batch) {
        import scala.concurrent.ExecutionContext.Implicits.global
        Future {
          if (StdIn.readLine != null) {
            println("--- requesting early termination (keyboard interrupt)")
            interrupt()
          } else {
            println("--- running without a stdin; please use batch mode")
          }
        }
      }

      config.finishBy.map({ time =>
        import scala.concurrent.ExecutionContext.Implicits.global
        Future {
          Thread.sleep(time - System.currentTimeMillis())
          println("--- request early termination (time limit)")
          interrupt()
        }
      })

      val result = Await.result(future, Duration.Inf)
      (result._1, result._2.size)
    }))

    println(s"--- In ${time}ms, found $numberFound based rings.")
    if (toResume.nonEmpty) {
      println(s"--- This enumeration was interrupted, so is not exhaustive. The following cases are incomplete: ")
      for (p <- toResume) {
        println(p)
      }
      if (config.resumable) {
        inFile.delete
        val io = new PrintWriter(new FileOutputStream(inFile))
        for (p <- toResume) {
          io.println(p)
        }
        io.close
      }
    } else {
      if (config.resumable) {
        inFile.delete
      }
    }

  }
}

