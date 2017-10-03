package net.tqft.toolkit.algebra

//import net.tqft.toolkit.algebra.ApproximateReals


trait PartiallyOrderedField[A] extends Field[A] with PartialOrdering[A] {
  
}


trait OrderedField[@specialized(Int, Long, Float, Double) A] extends PartiallyOrderedField[A] with OrderedEuclideanRing[A] { self =>
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

  // Be careful; this assumes exact arithmetic in the underlying field. This construct is for handling ignorance, not rounding.
  def intervalArithmetic[A: OrderedField]: PartiallyOrderedField[(A, A)] = {
    class IntervalArithmetic[A: OrderedField] extends PartiallyOrderedField[(A, A)] {
      val field = implicitly[OrderedField[A]]

      override def one: (A, A) = (field.one, field.one)
      override def zero: (A, A) = (field.zero, field.zero)

      override def zero_?(x: (A, A)) = field.compare(x._1, field.zero) <= 0 && field.compare(x._2, field.zero) >= 0

      override def partialCompare(x: (A,A), y: (A,A)): Option[Int] = {
        // not sure this is sane
        if(field.compare(x._2, y._1) < 0) {
          Some(-1)
        } else if(field.compare(y._2, x._1) < 0) {
          Some(1)
        } else if(x._1 == x._2 && x._2 == y._1 && y._1 == y._2) {
          Some(0)
        } else {
          None
        }
      }
      
      override def add(x: (A, A), y: (A, A)) = (field.add(x._1, y._1), field.add(x._2, y._2))
      override def negate(x: (A, A)): (A, A) = (field.negate(x._2), field.negate(x._1))

      override def multiply(x: (A, A), y: (A, A)): (A, A) = {
        val products = Seq(field.multiply(x._1, y._1), field.multiply(x._1, y._2),field.multiply(x._2, y._1),field.multiply(x._2, y._2))
        (field.minimum(products:_*), field.maximum(products:_*))
      }
      override def inverse(x: (A,A)) = inverseOption(x).get
      override def inverseOption(x: (A, A)): Option[(A, A)] = {
        if(zero_?(x)) {
          None
        } else {
          Some((field.inverse(x._2), field.inverse(x._1)))
        }
      }
    }
    new IntervalArithmetic[A]
  }
}