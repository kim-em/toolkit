package net.tqft.toolkit.algebra

import net.tqft.toolkit.arithmetic.Factor

import scala.language.implicitConversions

trait IntegerModel[@specialized(Int, Long) I] extends OrderedEuclideanRing[I] {
  def toBigInt(i: I): BigInt
  def fromBigInt(i: BigInt): I
  def from[II: IntegerModel](i: II): I = {
    fromBigInt(implicitly[IntegerModel[II]].toBigInt(i))
  }
}

trait ArbitraryPrecisionIntegerModel[I] extends IntegerModel[I]

object IntegerModel {
  implicit val integers = Integers
  implicit val longs = Longs
  implicit val bigIntegers = BigIntegers

  implicit def defaultFactorizationAlgorithm_[A](integers: IntegerModel[A]): Factorization[A] = new ECMFactorization.provideFactorization[A](integers)  
}

