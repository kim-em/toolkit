package net.tqft.toolkit.algebra.combinatorics

import net.tqft.toolkit.collections.Iterators.iterators2RichIterators
import scala.Array.canBuildFrom

case class PositiveSymmetricDecomposition(m: Array[Array[Int]]) {

  private def tailNorms(x: Array[Int]) = {
    var sum = 0
    (for (a <- x.reverse) yield {
      val r = sum
      sum += a * a
      r
    }).reverse
  }

  private val sqrtFloorCache = Array.tabulate(256)(sqrtFloorImpl)
  private def sqrtFloor(x: Int): Int = {
    if (x < 256) sqrtFloorCache(x) else sqrtFloorImpl(x)
  }
  private def sqrtFloorImpl(x: Int): Int = {
    (scala.math.sqrt(x) + 0.001).floor.toInt
  }
  private val sqrtCeilCache = Array.tabulate(256)(sqrtCeilImpl)
  private def sqrtCeil(x: Int): Int = {
    if (x < 256) sqrtCeilCache(x) else sqrtCeilImpl(x)
  }
  private def sqrtCeilImpl(x: Int): Int = {
    (scala.math.sqrt(x) - 0.001).ceil.toInt
  }

  def runtimeEstimators = PartialSolution(0, 0, Nil, IndexedSeq.empty, Nil).runtimeEstimators
  def numberOfDescendantsEstimators: Iterator[Long] = PartialSolution(0, 0, Nil, IndexedSeq.empty, Nil).numberOfDescendantsEstimators
  def numberOfDecompositionsEstimators: Iterator[Long] = PartialSolution(0, 0, Nil, IndexedSeq.empty, Nil).numberOfDecompositionsEstimators
  def randomPartialSolution =
    PartialSolution(0, 0, Nil, IndexedSeq.empty, Nil).randomLineage.toSeq.last._1.P.reverse.toArray

  def randomDecomposition = Iterator.continually(randomPartialSolution).filter(_.length == m.length).next

  def decompositions: Iterator[Array[Array[Int]]] = {
    PartialSolution(0, 0, Nil, IndexedSeq.empty, Nil).completions.filter({ A =>
      //      println("verifying: " + A.toList.map(_.toList))
      (for (i <- (0 until m.length).iterator; j <- i until m.length) yield {
        var s = m(i)(j)
        for (k <- 0 until A(i).length) {
          s -= A(i)(k) * A(j)(k)
        }
        s
      }).forall(_ == 0)
    })
  }

  case class PartialSolution(rows: Int, columns: Int, P: List[Array[Int]], columnHashes: IndexedSeq[Int], rowTailNorms: List[Array[Int]]) {
        println("PartialSolution(" + rows + ", " + columns + ", " + P.map(_.toList) + ", " + columnHashes + ", " + rowTailNorms.map(_.toList))
    def children: Iterator[PartialSolution] = {
      case class PartialRow(size: Int, r: List[Int], normGap: Int, innerProductGaps: List[Int]) {
        //        println(this)
        def children: Iterator[PartialRow] = {
          val column = P.map(_(size))

          val lower = {
            // (g - x * c) <= sqrt(normGap * rowTailNorm)
            // x >= 1/c (g - sqrt(normGap * rowTailNorm)

            var lower0 = 0
            val gapIter = innerProductGaps.iterator
            val columnIter = column.iterator
            val normIter = rowTailNorms.iterator
            while (gapIter.hasNext) {
              val g = gapIter.next
              val c = columnIter.next
              if (c != 0) {
                val r = (normIter.next).apply(size)
                val n = g - sqrtCeil(normGap * r)
                val d = n / c + (if (n % c == 0) 0 else 1)
                if (d > lower0) {
                  lower0 = d
                }
              }
            }

            lower0
          }
          val upper = {

            var upper0 = sqrtFloor(normGap)

            {
              val iter = innerProductGaps.iterator
              for (c <- column; g = iter.next; if c != 0; r = g / c; if r < upper0) upper0 = r
            }

            // but now, checking if we're in the same column group, and if so ensure we don't increase.
            if (size > 0 && columnHashes(size) == columnHashes(size - 1)) {
              scala.math.min(upper0, r.head)
            } else {
              upper0
            }
          }
          for (x <- (lower to upper).iterator) yield {
            PartialRow(size + 1, x :: r, normGap - x * x, column.zip(innerProductGaps).map(p => p._2 - x * p._1))
          }
        }
        def completions: Iterator[PartialSolution] = {
          if (size == columns) {
            if (innerProductGaps.forall(_ == 0)) {
              if (normGap == 0) {
                val completedRow = r.reverse.toArray
                Iterator(
                  PartialSolution(
                    rows + 1,
                    columns,
                    completedRow :: P,
                    columnHashes.zip(completedRow).map(_.hashCode),
                    tailNorms(completedRow) :: rowTailNorms))
              } else {
                // take a sum of squares decomposition of the norm gap
                for (ss <- SumOfSquaresDecompositions(normGap)) yield {
                  val tail = ss.flatMap(p => Seq.fill(p._2)(p._1))
                  val pad = Array.fill(tail.size)(0)
                  val completedRow = ((r.reverse ++ tail).toArray)
                  PartialSolution(
                    rows + 1,
                    columns + tail.size,
                    completedRow :: P.map(_ ++ pad),
                    (columnHashes ++ Seq.fill(tail.size)(rows)).zip(completedRow).map(_.hashCode),
                    tailNorms(completedRow) :: rowTailNorms.map(row => row.padTo(completedRow.length, row.last)))
                }
              }
            } else {
              // TODO once we have Cauchy-Schwarz, this shouldn't happen!
              Iterator.empty
            }
          } else {
            children.flatMap(_.completions)
          }
        }
      }

      if (rows == m.length) {
        Iterator.empty
      } else {
        PartialRow(0, Nil, m(rows)(rows), m(rows).take(rows).toList.reverse).completions
      }
    }
    def completions: Iterator[Array[Array[Int]]] = {
      if (rows == m.length) {
        Iterator(P.reverse.toArray)
      } else {
        children.flatMap(_.completions)
      }
    }

    def randomLineage: Iterator[(PartialSolution, Seq[PartialSolution], Long)] = {
      import net.tqft.toolkit.collections.Iterators._
      val (t, nc) = net.tqft.toolkit.Profiler.timing(this.children.toList)
      Iterator.iterateUntilNone((this, nc, t))({
        case (_, c, _) if c.nonEmpty => {
          val n = c(scala.util.Random.nextInt(c.size))
          val (t, nc) = net.tqft.toolkit.Profiler.timing(n.children.toList)
          Some((n, nc, t))
        }
        case _ => None
      })
    }
    def numberOfDescendantsEstimators: Iterator[Long] = {
      def estimate = randomLineage.map(_._2.size).filterNot(_ == 0).product
      def estimates = Iterator.continually(estimate)
      def partialSums = estimates.scanLeft(0L)(_ + _)
      def averages = partialSums.zipWithIndex.collect({ case (s, i) if i != 0 => s / i })
      averages
    }
    // this might be completely bogus:
    def numberOfDecompositionsEstimators: Iterator[Long] = {
      def estimate = {
        val l = randomLineage.toStream
        if (l.last._1.rows == m.length) {
          l.map(_._2.size).filterNot(_ == 0).product
        } else {
          0
        }
      }
      def estimates = Iterator.continually(estimate)
      def partialSums = estimates.scanLeft(0L)(_ + _)
      def averages = partialSums.zipWithIndex.collect({ case (s, i) if i != 0 => s / i })
      averages
    }
    // measured in days!
    def runtimeEstimators: Iterator[Double] = {
      def estimate = randomLineage.foldLeft((1.0, 0.0))({ (a, b) => (a._1 * b._2.size, a._2 + a._1 * b._3 / 1000.0) })._2
      def estimates = Iterator.continually(estimate)
      def partialSums = estimates.scanLeft(0.0)(_ + _ / 86400)
      def averages = partialSums.zipWithIndex.collect({ case (s, i) if i != 0 => s / i })
      averages
    }
  }

}