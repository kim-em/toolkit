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

