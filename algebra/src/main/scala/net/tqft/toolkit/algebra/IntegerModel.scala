package net.tqft.toolkit.algebra

import net.tqft.toolkit.arithmetic.Factor

trait IntegerModel[@specialized(Int, Long) I] extends OrderedEuclideanRing[I] with Factorization[I] {
  def toBigInt(i: I): BigInt
  def fromBigInt(i: BigInt): I
  def from[II: IntegerModel](i: II): I = {
    fromBigInt(implicitly[IntegerModel[II]].toBigInt(i))
  }
  override def factor(x: I) = {
    Factor(toBigInt(x)).groupBy(x => x).map(p => fromBigInt(p._1) -> p._2.size)
  }
}

trait ArbitraryPrecisionIntegerModel[I] extends IntegerModel[I]

object IntegerModel {
  implicit val integers = Integers
  implicit val longs = Longs
  implicit val bigIntegers = BigIntegers
}

