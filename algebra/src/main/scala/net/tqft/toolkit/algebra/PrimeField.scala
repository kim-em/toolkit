package net.tqft.toolkit.algebra

object PrimeField {
  def apply[I: IntegerModel](p: I): Field[I] with Finite[I] = {
    def integers = implicitly[IntegerModel[I]]
    p match {
      case p: Int if integers == Integers => require(p < scala.math.sqrt(Integer.MAX_VALUE))
    }
    require(integers.toBigInt(p).isProbablePrime(20))
    new Field[I] with Finite[I] {
      override def elements = (0 until Integers.from(p)).map(integers.fromInt).toSet
      override def inverse(x: I) = {
        if (x == integers.zero) throw new ArithmeticException("/ by zero")
        integers.extendedEuclideanAlgorithm(x, p)._1
      }
      override def negate(x: I) = integers.remainder(integers.negate(x), p)
      override val zero = integers.zero
      override val one = integers.one
      override def multiply(x: I, y: I) = integers.remainder(integers.multiply(x, y), p)
      override def add(x: I, y: I) = integers.remainder(integers.add(x, y), p)
      override def fromInt(x: Int) = integers.remainder(integers.fromInt(x), p)
    }
  }
}
