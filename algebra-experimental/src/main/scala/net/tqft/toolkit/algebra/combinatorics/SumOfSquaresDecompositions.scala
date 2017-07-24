package net.tqft.toolkit.algebra.combinatorics

object SumOfSquaresDecompositions {
  private val cache = scala.collection.mutable.Map[Int, Seq[Seq[(Int, Int)]]]()
  /**
   * returns an iterator of {(a_i, b_i)}_i, with \sum b_i a_i^2 = n
   */
  def apply(n: Int): Iterator[Seq[(Int, Int)]] = {
    def impl: Iterator[Seq[(Int, Int)]] = {
      def sqrt(k: Int) = {
        if (k <= 0) {
          0
        } else {
          val closest = scala.math.sqrt(k).round.intValue
          if (closest * closest > k) {
            closest - 1
          } else {
            closest
          }
        }
      }

      def extend(limit: Int, remainder: Int, partial: Seq[(Int, Int)]): Iterator[Seq[(Int, Int)]] = {
        limit match {
          case 0 => {
            remainder match {
              case 0 => Iterator(partial)
              case _ => Iterator.empty
            }
          }
          case 1 => Iterator((1, remainder) +: partial)
          case limit => {
            for (
              b <- (0 to (remainder / (limit * limit))).iterator;
              next = (limit, b) +: partial;
              nextRemainder = remainder - b * limit * limit;
              nextLimit = scala.math.min(limit - 1, sqrt(nextRemainder));
              result <- extend(nextLimit, nextRemainder, next)
            ) yield result
          }
        }
      }

      extend(sqrt(n), n, Nil)
    }

    if (n < 20) {
      cache.getOrElseUpdate(n, impl.toSeq).iterator
    } else {
      impl
    }
  }

}