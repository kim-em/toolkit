package net.tqft.toolkit.algebra

trait DivisionRing[A] extends EuclideanDomain[A] with Group[A] {
  override def quotientRemainder(x: A, y: A) = (multiply(x, inverse(y)), zero)
  override def remainder(x: A, y: A) = zero
  def quotientByInt(x: A, y: Int): A = quotient(x, fromInt(y))
  def fromRational(x: Fraction[Int]) = quotient(fromInt(x.numerator), fromInt(x.denominator))
}

// there's not much to say here; the only additional requirement to be a field is commutativity, but the type system doesn't see that.
trait Field[A] extends DivisionRing[A]

trait ImplicitFields extends ImplicitEuclideanDomains {
  implicit def forget[A: OrderedField]: Field[A] = implicitly[OrderedField[A]]

  implicit def fieldOfFractions[A:EuclideanDomain]: Field[Fraction[A]] = Fields.fieldOfFractions(implicitly[EuclideanDomain[A]])
}

object Field extends ImplicitFields