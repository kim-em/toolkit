package net.tqft.toolkit.algebra

trait OrderedField[@specialized(Int, Long, Float, Double) A] extends Field[A] with OrderedEuclideanRing[A] { self =>
  def chop(x: A, epsilon: A): A = {
    if (compare(abs(x), epsilon) < 0) zero else x
  }
}

object OrderedField {
  implicit val Doubles: OrderedField[Double] = Gadgets.Doubles
  implicit def fieldOfFractions[A: OrderedEuclideanRing]: OrderedField[Fraction[A]] = Fields.fieldOfFractions(implicitly[OrderedEuclideanRing[A]])
}