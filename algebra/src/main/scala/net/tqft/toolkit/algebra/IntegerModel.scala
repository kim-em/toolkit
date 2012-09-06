package net.tqft.toolkit.algebra

trait IntegerModel[@specialized(Int, Long) I] extends OrderedEuclideanRing[I] {
  def toBigInt(i: I): BigInt
  def fromBigInt(i: BigInt): I
  def from[II: IntegerModel](i: II): I = {
    fromBigInt(implicitly[IntegerModel[II]].toBigInt(i))
  }
}

object IntegerModel {
  implicit val Integers: IntegerModel[Int] = Gadgets.Integers
  implicit val BigInts: IntegerModel[BigInt] = Gadgets.BigIntegers
}
