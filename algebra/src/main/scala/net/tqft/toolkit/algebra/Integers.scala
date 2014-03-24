package net.tqft.toolkit.algebra

object Integers extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.IntIsIntegral) {
  override def toBigInt(i: Int) = BigInt(i)
  override def fromBigInt(b: BigInt) = b.ensuring(_.isValidInt).intValue
}
object Longs extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.LongIsIntegral) {
  override def toBigInt(i: Long) = BigInt(i)
  override def fromBigInt(b: BigInt) = b.ensuring(_ <= Long.MaxValue).ensuring(_ >= Long.MinValue).longValue
}
object BigIntegers extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.BigIntIsIntegral) {
  override def toBigInt(i: BigInt) = i
  override def fromBigInt(b: BigInt) = b
}