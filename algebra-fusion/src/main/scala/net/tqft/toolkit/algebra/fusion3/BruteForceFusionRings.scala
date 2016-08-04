package net.tqft.toolkit.algebra.fusion3

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
import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentHashMap

object BruteForceFusionRings extends App {

  case class Config(
    selfDualObjects: Int = 5,
    dualPairs: Int = 0,
    globalDimensionBound: Double = 60.0,
    umtc: Boolean = false,
    minimumDimension: Option[Double] = None,
    withFunctor: Option[(Array[Array[Int]], Array[Array[Array[Int]]])] = None,
    withMatrix: Option[Array[Array[Int]]] = None,
    resumable: Boolean = false,
    finishBy: Option[Long] = None,
    batch: Boolean = false,
    force: Boolean = false)

  val parser = new scopt.OptionParser[Config]("BruteForceFusionRings") {
    head("BruteForceFusionRings", "1.0")
    opt[Unit]('u', "umtc") action { (_, c) =>
      c.copy(umtc = true)
    } text ("only search for rings which could be the fusion ring of a unitary modular tensor category")
    opt[Double]('m', "minimum") valueName ("<minimum-squared-dimension>") action { (x, c) =>
      c.copy(minimumDimension = Some(x))
    } text ("all non-trivial objects must have a minimum squared dimension")
    opt[Unit]('r', "resumable") action { (_, c) =>
      c.copy(resumable = true)
    } text ("resume existing work, if available, and checkpoint resumable data on <enter>")
    opt[String]('z', "functor") valueName ("<induction-matrix>:<target-ring>") action { (x, c) =>
      val Seq(inductionString, ringString) = x.split(":").toSeq
      val inductionEntries = if (inductionString.contains(",")) inductionString.split(",").map(_.toInt) else inductionString.toCharArray.map(_.toString.toInt)
      val ringEntries = if (ringString.contains(",")) ringString.split(",").map(_.toInt) else ringString.toCharArray.map(_.toString.toInt)
      val targetRank = scala.math.round(scala.math.pow(ringEntries.length, 1.0 / 3)).toInt
      val induction = inductionEntries.grouped(inductionEntries.length / targetRank).toArray
      val ring = ringEntries.grouped(targetRank * targetRank).map(_.grouped(targetRank).toArray).toArray
      c.copy(withFunctor = Some((induction, ring)), resumable = false)
    } text ("compatible with a specified homomorphism to a specified ring")
    opt[String]('x', "matrix") valueName ("<matrix>") action { (x, c) =>
      val matrixEntries = if (x.contains(",")) x.split(",").map(_.toInt) else x.toCharArray.map(_.toString.toInt)
      val matrix = matrixEntries.grouped(scala.math.sqrt(matrixEntries.size).round.toInt).toArray
      c.copy(withMatrix = Some(matrix), resumable = false)
    } text ("with specified first object")
    opt[Double]('h', "hours") valueName ("<hours>") action { (x, c) =>
      c.copy(finishBy = Some(System.currentTimeMillis() + (x * 60 * 60 * 1000).toLong))
    } text ("run for at most <hours> hours")
    opt[Unit]('q', "batch") action { (_, c) =>
      c.copy(batch = true)
    } text ("disable keyboard interrupt")
    opt[Unit]('f', "force") action { (_, c) =>
      c.copy(force = true)
    } text ("overwrite existing data files")
    help("help") text ("prints this usage text")
    arg[Int]("<number-of-self-dual-objects>") hidden () action { (x, c) => c.copy(selfDualObjects = x) }
    arg[Int]("<number-of-dual-pairs>") hidden () action { (x, c) => c.copy(dualPairs = x) }
    arg[Double]("<global-dimension-bound>") hidden () action { (x, c) => c.copy(globalDimensionBound = x) }
  }

  parser.parse(args, Config()) map { config =>
    require(!config.resumable || (config.withFunctor.isEmpty && config.withMatrix.isEmpty))

    val enumeration = Enumeration(config.selfDualObjects, config.dualPairs, config.globalDimensionBound, config.umtc, config.minimumDimension, config.withFunctor, config.withMatrix)

    val suffix = (if(config.umtc) "u" else "") + (if(config.minimumDimension.nonEmpty) "m" + config.minimumDimension.get else "")
    
    val dir = "fusion-rings3" + suffix + "/"
    new File(dir).mkdir
    val prefix = dir + config.selfDualObjects + "," + config.dualPairs + "," + config.globalDimensionBound

    val inFile = new File(prefix + ".resume")
    val partialFile = new File(prefix + ".partial")
    val completeFile = new File(prefix + ".classification")

    if (config.resumable) {
      if (config.force) {
        inFile.delete
        partialFile.delete
        completeFile.delete
      }

      if ((inFile.exists == partialFile.exists) && !completeFile.exists ||
        !inFile.exists && !partialFile.exists && completeFile.exists) {
        // looks good
      } else {
        println("--- resume files look wrong, cleaning up!")
        inFile.delete
        partialFile.delete
        completeFile.delete
      }
    }

    val counter = new AtomicInteger(0)

    if (config.resumable && completeFile.exists) {
      println("--- nothing to do...")
    } else {

      val notify: enumeration.Complete => Unit = {
        val seen = {
          import scala.collection.JavaConverters._
          java.util.Collections.newSetFromMap(new ConcurrentHashMap[String, java.lang.Boolean]()).asScala
        }

        val pw = new PrintWriter(new BufferedWriter(new FileWriter(partialFile, true)));
        { c =>
          val s0 = c.canonicalize.toString
          val s1 = s0.split(" ").init.mkString(" ") // throw out the global dimension estimate
          synchronized {
            if (!seen.contains(s1)) {
              seen += s1
              counter.incrementAndGet
              println(s0)
              if (config.resumable) {
                if (seen.size > 10000) seen.retain({ t => Random.nextInt % 4 > 0 })
                pw.println(s0)
                pw.flush
              }
            }
          }
        }
      }

      val (time, (toResume, numberFound)) = (net.tqft.toolkit.Profiler.timing({
        val targets: Seq[enumeration.Partial] = if (config.resumable && inFile.exists) {
          Source.fromFile(inFile).getLines.map(enumeration.Partial.apply).toSeq
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
            (results.iterator.flatMap(_._1), results.flatMap(_._2))
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

      println(s"--- In ${time}ms, found ${counter.get} based rings.")
      if (toResume.nonEmpty) {
        println(s"--- This enumeration was interrupted, so is not exhaustive. The following cases are incomplete: ")
        if (config.resumable) {
          inFile.delete
          val io = new PrintWriter(new FileOutputStream(inFile))
          for (p <- toResume) {
            println(p)
            io.println(p)
          }
          io.close
        } else {
          for (p <- toResume) {
            println(p)
          }
        }
      } else {
        if (config.resumable) {
          inFile.delete
          partialFile.renameTo(completeFile)
        }
      }
      //      System.exit(0)
    }
  }
}

