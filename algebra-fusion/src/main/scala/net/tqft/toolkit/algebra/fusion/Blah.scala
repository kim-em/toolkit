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

object Blah extends App {
  val rank = 5

  val multiplicities = for (i <- 1 until rank; j <- 1 until rank; k <- 1 until rank) yield Seq(i, j, k)
  val representativeMultiplicities = {
    //    import Ordering.Implicits._
    multiplicities.filter(m => m == m.sorted)
  }
  val numberOfVariables = representativeMultiplicities.size
  val lookupTable = for (m <- multiplicities) yield representativeMultiplicities.indexOf(m.sorted)
  def lookup(i: Int, j: Int, k: Int) = {
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
  val lookupDiagonals = (for (i <- 1 until rank) yield representativeMultiplicities.indexOf(Seq(i, i, i))).sorted

  def N(x: Array[Int])(i: Int, j: Int, k: Int) = {
    lookup(i, j, k) match {
      case Left(m) => m
      case Right(z) => x(z)
    }
  }

  object Partial {
    val root = {
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

    def apply(s: String): Partial = {
      val n = if (s.contains(',')) {
        s.split(',').map(_.toInt)
      } else {
        s.toCharArray().map(_.toInt)
      }
      n.foldLeft(root)({ (p, m) => p.next(m).get })
    }
  }

  case class Partial(step: Int, x: Array[Int], r: Array[Array[Int]], hint: Array[Double] = Array.fill(rank)(1.0)) {
    override def toString = {
      val sep = if (x.max > 9) "," else ""
      x.take(step + 1).mkString(sep)
    }

    def next(m: Int): Option[Partial] = {

      val nextX = x.clone()
      nextX(step + 1) = m

      if (inequalities(step + 1, nextX)) {
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
      } else {
        None
      }
    }

    def associative_? = associativeAtStep_?(step)(x)

    def children = {
      Iterator.from(0).map(next).takeWhile(_.nonEmpty).map(_.get).filter(_.associative_?).toSeq
    }

    def done_? = step == numberOfVariables - 1

    def descendants: Seq[Partial] = {
      if (done_?) {
        Seq(this)
      } else {
        children.par.flatMap(_.descendants).seq
      }
    }

    def interruptibleDescendants(notify: Partial => Unit = { p: Partial => () }): (Future[(Seq[Partial], Seq[Partial])], () => Unit) = {
      val threads = 10

      val latch = new AtomicInteger(1)
      val activeWorkers = new AtomicInteger(0)
      val stack = new ConcurrentLinkedDeque[Partial]
      val finished = new ConcurrentLinkedDeque[Partial]

      stack.push(this)

      def requestTermination {
        latch.decrementAndGet
      }

      import scala.concurrent.ExecutionContext.Implicits.global

      //      implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

      def work: Future[Unit] = {
        (Future {
          if (latch.get > 0) {
            val p = stack.poll
            if (p == null) {
              activeWorkers.decrementAndGet
              Future.successful(())
            } else {
              if (p.done_?) {
                notify(p)
                finished.push(p)
                work
              } else {
                for (c <- p.children) stack.push(c)
                val a = activeWorkers.getAndSet(threads)
                Future.sequence(Seq.fill(Seq(threads - a, 0).max + 1)(work)).map(_ => ())
              }
            }
          } else {
            activeWorkers.decrementAndGet
            Future.successful(())
          }
        }).flatMap(x => x)
      }

      (work.map(_ => (stack.toArray.toSeq.asInstanceOf[Seq[Partial]], finished.toArray.toSeq.asInstanceOf[Seq[Partial]])), requestTermination _)
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

  //  println(associativitiesByVariable.map(_.size))

  val globalDimensionBound = 60.0

//  while (true) {
    //    println(Profiler.timing(Partial.root.descendants.size))
    println(Profiler.timing({
      val (future, interrupt) = Partial.root.interruptibleDescendants(println)

      import scala.concurrent.ExecutionContext.Implicits.global

      Future {
        StdIn.readLine
        interrupt()
      }
      val result = Await.result(future, Duration.Inf)
      (result._1.size, result._2.size)
    }))
//  }

}