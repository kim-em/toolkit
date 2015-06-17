package net.tqft.toolkit.algebra

object Integers extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.IntIsIntegral) {
  override def toBigInt(i: Int) = BigInt(i)
  override def fromBigInt(b: BigInt) = b.ensuring(_.isValidInt).intValue
  
  override def toString = "Integers"
}
object Longs extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.LongIsIntegral) {
  override def toBigInt(i: Long) = BigInt(i)
  override def fromBigInt(b: BigInt) = b.ensuring(_ <= Long.MaxValue).ensuring(_ >= Long.MinValue).longValue

  override def toString = "Longs"
}
object BigIntegers extends NumericTypes.IntegralEuclideanRing(scala.math.Numeric.BigIntIsIntegral) with ArbitraryPrecisionIntegerModel[BigInt] {
  override def toBigInt(i: BigInt) = i
  override def fromBigInt(b: BigInt) = b
  
  override def gcd(x: BigInt, y: BigInt) = {
    val builtInGCD = x.gcd(y)
    val result = if(y.signum * builtInGCD.signum == 1) {
      builtInGCD
    } else {
      - builtInGCD
    }
    result//.ensuring(_ == super.gcd(x, y))
  }
  override def toString = "BigIntegers"
}