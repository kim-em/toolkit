package net.tqft.toolkit.algebra

trait OrderedField[@specialized(Int, Long, Float, Double) A] extends Field[A] with OrderedEuclideanDomain[A] { self =>
  def chop(x: A, epsilon: A): A = {
    if (compare(abs(x), epsilon) < 0) zero else x
  }
}

trait ImplicitOrderedFields {
  implicit val Doubles: OrderedField[Double] = Gadgets.Doubles

  implicit def fieldOfFractions[A: OrderedEuclideanDomain]: OrderedField[Fraction[A]] = Fields.fieldOfFractions(implicitly[OrderedEuclideanDomain[A]])
}

object OrderedField extends ImplicitOrderedFields