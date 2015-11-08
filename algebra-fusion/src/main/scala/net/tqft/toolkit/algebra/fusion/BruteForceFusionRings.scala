package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import net.tqft.toolkit.algebra.enumeration.Odometer
import net.tqft.toolkit.Profiler
import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.Future
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.CyclicBarrier
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
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
        val pw = new PrintWriter(new BufferedWriter(new FileWriter(outFile, true)));
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

case class Enumeration(selfDualObjects: Int, dualPairs: Int, globalDimensionBound: Double) {
  val rank = selfDualObjects + 2 * dualPairs
  require(dualPairs == 0)

  private val multiplicities = for (i <- 1 until rank; j <- 1 until rank; k <- 1 until rank) yield Seq(i, j, k)
  private val representativeMultiplicities = {
    multiplicities.filter(m => m == m.sorted)
  }
  private val numberOfVariables = representativeMultiplicities.size
  private val lookupTable = for (m <- multiplicities) yield representativeMultiplicities.indexOf(m.sorted)

  private def lookup(i: Int, j: Int, k: Int) = {
    if (i == 0) {
      if (j == k) Left(1) else Left(0)
    } else {
      if (j == 0) {
        if (i == k) Left(1) else Left(0)
      } else {
        if (k == 0) {
          if (i == j) Left(1) else Left(0)
        } else {
          Right(lookupTable((i - 1) * (rank - 1) * (rank - 1) + (j - 1) * (rank - 1) + (k - 1)))
        }
      }
    }
  }
  private val lookupDiagonals = (for (i <- 1 until rank) yield representativeMultiplicities.indexOf(Seq(i, i, i))).sorted

  def N(x: Array[Int])(i: Int, j: Int, k: Int) = {
    lookup(i, j, k) match {
      case Left(m) => m
      case Right(z) => x(z)
    }
  }

  lazy val root = {
    val zeroes = Array.fill(numberOfVariables)(0)
    val r = {
      Array.tabulate(rank, rank)({ (i, j) =>
        var t = 0
        for ((k, l) <- Rterms(0)(i)(j)) {
          t = t + N(zeroes)(k, j, l) * N(zeroes)(k, l, i)
        }
        t
      })
    }
    Partial(-1, zeroes, r)
  }

  case class Complete(m: Seq[Seq[Seq[Int]]], r: Seq[Seq[Int]], globalDimensionEstimate: Double) {
    override def toString = {
      val max = m.map(_.map(_.max)).map(_.max).max
      val sep = if (max > 9) "," else ""
      selfDualObjects + "," + dualPairs + " " + max + " " + m.map(_.flatten).flatten.mkString(sep) + " " + globalDimensionEstimate
    }

    lazy val dualData: Seq[Int] = {
      for (i <- 0 until rank) yield {
        m(i).map(_.head).indexOf(1)
      }
    }

  }

  object Partial {
    def apply(s: String): Partial = {
      val numbers = if (s.contains(",")) {
        s.split(',').map(_.toInt)
      } else {
        s.toCharArray().map(_.toString.toInt)
      }
      numbers.foldLeft(root)(_.next(_).get)
    }
  }

  case class Partial(step: Int, x: Array[Int], r: Array[Array[Int]], hint: Array[Double] = Array.fill(rank)(1.0)) {
    override def toString = {
      val sep = if (x.max > 9) "," else ""
      x.take(step + 1).mkString(sep)
    }

    def complete: Option[Complete] = {
      if (done_?) {
        val m = Seq.tabulate(rank, rank, rank)({ (i, j, k) => N(x)(i, j, k) })
        Some(Complete(m, r.map(_.toSeq).toSeq, FrobeniusPerronEigenvalues.estimate(r)))
      } else {
        None
      }
    }

    def next(m: Int): Option[Partial] = {
      require(m < 25)

      val nextX = x.clone()
      nextX(step + 1) = m

      if (inequalities(step + 1, nextX)) {

        if (m == 0) {
          Some(Partial(step + 1, x, r, hint))
        } else {

          val nextR = Array.tabulate(rank, rank)({ (i, j) =>
            var t = r(i)(j)
            for ((k, l) <- Rterms(step + 2)(i)(j)) {
              t = t + N(nextX)(k, j, l) * N(nextX)(k, l, i)
            }
            t
          })

          val (eigenvalue, nextHint) = FrobeniusPerronEigenvalues.estimateWithEigenvector(nextR, 0.001, globalDimensionBound + 1, Some(hint))

          if (eigenvalue <= globalDimensionBound) {
            Some(Partial(step + 1, nextX, nextR, nextHint))
          } else {
            None
          }
        }
      } else {
        None
      }
    }

    def associative_? = associativeAtStep_?(step)(x)

    def children = {
      Iterator.from(0).map(next).takeWhile(_.nonEmpty).map(_.get).filter(_.associative_?).toSeq
    }

    def done_? = step == numberOfVariables - 1

    def descendants: Seq[Complete] = {
      if (done_?) {
        complete.toSeq
      } else {
        children.par.flatMap(_.descendants).seq
      }
    }

    def interruptibleDescendants(notify: Complete => Unit = { p => () }): (Future[(Seq[Partial], Seq[Complete])], () => Unit) = {
      val latch = new AtomicInteger(1)
      def interrupt {
        latch.decrementAndGet
      }

      import scala.concurrent.ExecutionContext.Implicits.global

      (Future { interruptibleDescendantsWorker(latch, notify) }, interrupt _)
    }

    private def interruptibleDescendantsWorker(latch: AtomicInteger, notify: Complete => Unit): (Seq[Partial], Seq[Complete]) = {
      if (latch.get > 0) {
        //        println("latch open, working on " + this)
        complete match {
          case Some(c) => {
            notify(c)
            (Seq.empty, Seq(c))
          }
          case None => {
            val results = children.par.map(_.interruptibleDescendantsWorker(latch, notify)).seq
            (results.flatMap(_._1), results.flatMap(_._2))
          }
        }
      } else {
//        println("latch closed, reporting " + this)
        (Seq(this), Seq.empty)
      }
    }

  }

  def inequalities(step: Int, x: Array[Int]): Boolean = {
    val v = representativeMultiplicities(step)
    if (v(0) > 1 && v(0) == v(1) && v(1) == v(2)) {
      x(step) <= x(lookup(v(0) - 1, v(0) - 1, v(0) - 1).right.get)
    } else {
      true
    }
  }

  val Rterms = {
    import net.tqft.toolkit.arithmetic.MinMax._

    val terms = for (i <- 0 until rank; j <- 0 until rank; k <- 0 until rank; l <- 0 until rank) yield {
      (i, j, (k, l), Seq(lookup(k, j, l), lookup(k, l, i)).collect({ case Right(z) => z }).maxOption.getOrElse(-1))
    }

    val map = terms.groupBy(_._4).mapValues(_.groupBy(_._1).mapValues(_.groupBy(_._2)))

    IndexedSeq.tabulate(numberOfVariables + 1, rank, rank)({ (s, i, j) =>
      map.get(s - 1) match {
        case None => IndexedSeq.empty
        case Some(map) => map.get(i) match {
          case None => IndexedSeq.empty
          case Some(map) => map.getOrElse(j, IndexedSeq.empty).map(_._3)
        }
      }
    })
  }

  def associativeAtStep_?(step: Int)(m: Array[Int]): Boolean = {
    for ((x, y, z, b) <- associativitiesByVariable(step)) {
      if (!associativeAt_?(x, y, z, b)(m)) return false
    }
    true
  }
  def associativeAt_?(x: Int, y: Int, z: Int, b: Int)(m: Array[Int]): Boolean = {
    var t = 0
    for (a <- (0 until rank)) {
      t = t + N(m)(x, y, a) * N(m)(a, z, b) - N(m)(x, a, b) * N(m)(y, z, a)
    }
    t == 0
  }
  val associativitiesByVariable = {
    val equations = for (x <- 1 until rank; y <- x until rank; z <- x until rank; b <- x until rank) yield {
      val p = (for (a <- 1 until rank) yield {
        Seq(lookup(x, y, a), lookup(a, z, b), lookup(x, a, b), lookup(y, z, a)).collect({ case Right(z) => z }).max
      }).max
      ((x, y, z, b), p)
    }
    val map = equations.groupBy(_._2).mapValues(_.map(_._1)).withDefaultValue(IndexedSeq.empty)
    IndexedSeq.tabulate(numberOfVariables)(map)
  }

}
  
