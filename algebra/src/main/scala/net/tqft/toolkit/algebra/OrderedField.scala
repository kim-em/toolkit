package net.tqft.toolkit.algebra

//import net.tqft.toolkit.algebra.ApproximateReals

trait OrderedField[@specialized(Int, Long, Float, Double) A] extends Field[A] with OrderedEuclideanRing[A] { self =>
  def chop(x: A, epsilon: A): A = {
    if (compare(abs(x), epsilon) < 0) zero else x
  }
}

object OrderedField {
  class OrderedFieldOfFractions[A: OrderedEuclideanRing] extends Field.FieldOfFractions[A] with OrderedField[Fraction[A]] {
    override def ring = implicitly[OrderedEuclideanRing[A]]
    def compare(x: Fraction[A], y: Fraction[A]) = ring.compare(ring.multiply(x.numerator, y.denominator), ring.multiply(y.numerator, x.denominator))
  }

  //  implicit def forget[A:ApproximateReals]: OrderedField[A] = implicitly[ApproximateReals[A]]
  implicit def fieldOfFractions[A: OrderedEuclideanRing]: OrderedField[Fraction[A]] = new OrderedFieldOfFractions[A]
  implicit object fieldOfDoubles extends NumericTypes.FractionalField(scala.math.Numeric.DoubleIsFractional) 
}