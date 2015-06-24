package net.tqft.toolkit.algebra

object Integers extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.IntIsIntegral) {
  override def toBigInt(i: Int) = BigInt(i)
  override def fromBigInt(b: BigInt) = b.ensuring(_.isValidInt).intValue

  override def zero_?(i: Int) = i == 0
  override def one_?(i: Int) = i == 1

  override def toString = "Integers"
}
object Longs extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.LongIsIntegral) {
  override def toBigInt(i: Long) = BigInt(i)
  override def fromBigInt(b: BigInt) = b.ensuring(_ <= Long.MaxValue).ensuring(_ >= Long.MinValue).longValue

  override def zero_?(i: Long) = i == 0L
  override def one_?(i: Long) = i == 1L

  override def toString = "Longs"
}
object BigIntegers extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.BigIntIsIntegral) with ArbitraryPrecisionIntegerModel[BigInt] {
  override def toBigInt(i: BigInt) = i
  override def fromBigInt(b: BigInt) = b

  override def gcd(x: BigInt, y: BigInt) = {
    val builtInGCD = x.gcd(y)
    val result = if (y.signum * builtInGCD.signum == 1) {
      builtInGCD
    } else {
      -builtInGCD
    }
    result //.ensuring(_ == super.gcd(x, y))
  }

  override def zero_?(i: BigInt) = i == java.math.BigInteger.ZERO
  override def one_?(i: BigInt) = i == java.math.BigInteger.ONE

  override def toString = "BigIntegers"
}