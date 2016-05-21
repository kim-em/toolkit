package net.tqft.toolkit.algebra

import java.util.Comparator

object NumericTypes {
  class NumericRing[T](numeric: Numeric[T]) extends Ring[T] with Comparator[T] {
    override def multiply(x: T, y: T) = numeric.times(x, y)
    override def add(x: T, y: T) = numeric.plus(x, y)
    override def negate(x: T) = numeric.negate(x)

    override def fromInt(x: Int) = numeric.fromInt(x)
    override val one = numeric.one
    override val zero = numeric.zero

    override def compare(x: T, y: T) = numeric.compare(x, y)
  }

  abstract class IntegralEuclideanRing[T](numeric: Integral[T]) extends NumericRing(numeric) with IntegerModel[T] {
    override def quotientRemainder(x: T, y: T) = (numeric.quot(x, y), numeric.rem(x, y))
    override def quotient(x: T, y: T) = numeric.quot(x, y)
    override def remainder(x: T, y: T) = numeric.rem(x, y)
    override def toBigInt(t: T) = BigInt(t.toString)
  }

  class FractionalField[T](numeric: Fractional[T]) extends NumericRing(numeric) with OrderedField[T] {
    override def inverse(x: T) = numeric.div(one, x)
    override def quotientRemainder(x: T, y: T) = (numeric.div(x, y), zero)
    override def quotient(x: T, y: T) = numeric.div(x, y)
    override def remainder(x: T, y: T) = zero
  }
}