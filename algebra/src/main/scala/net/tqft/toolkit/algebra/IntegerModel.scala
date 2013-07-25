package net.tqft.toolkit.algebra

trait IntegerModel[@specialized(Int, Long) I] extends OrderedEuclideanRing[I] {
  def toBigInt(i: I): BigInt
  def fromBigInt(i: BigInt): I
  def from[II: IntegerModel](i: II): I = {
    fromBigInt(implicitly[IntegerModel[II]].toBigInt(i))
  }
}

object IntegerModel {
  implicit val integers = Integers
  implicit val longs = Longs
  implicit val bigIntegers = BigIntegers
}

object Integers extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.IntIsIntegral) {
  override def toBigInt(i: Int) = BigInt(i)
  override def fromBigInt(b: BigInt) = b.ensuring(_.isValidInt).intValue

  private val sumOfSquaresCache = scala.collection.mutable.Map[Int, Seq[Seq[(Int, Int)]]]()
  /**
   * returns an iterator of {(a_i, b_i)}_i, with \sum b_i a_i^2 = n
   */
  def sumOfSquaresDecomposition(n: Int): Seq[Seq[(Int, Int)]] = {
    def impl: Seq[Seq[(Int, Int)]] = {
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

      def extend(limit: Int, remainder: Int, partial: Seq[(Int, Int)]): Seq[Seq[(Int, Int)]] = {
        limit match {
          case 0 => {
            remainder match {
              case 0 => Seq(partial)
              case _ => Seq.empty
            }
          }
          case 1 => Seq((1, remainder) +: partial)
          case limit => {
            for (
              b <- (0 to (remainder / (limit * limit)));
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
      sumOfSquaresCache.getOrElseUpdate(n, impl)
    } else {
      impl
    }
  }

}
object Longs extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.LongIsIntegral) {
  override def toBigInt(i: Long) = BigInt(i)
  override def fromBigInt(b: BigInt) = b.ensuring(_ <= Long.MaxValue).ensuring(_ >= Long.MinValue).longValue
}

object BigIntegers extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.BigIntIsIntegral) {
  override def toBigInt(i: BigInt) = i
  override def fromBigInt(b: BigInt) = b
}