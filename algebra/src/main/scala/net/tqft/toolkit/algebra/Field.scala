package net.tqft.toolkit.algebra

trait DivisionRing[@specialized(Float, Double) A] extends EuclideanRing[A] with Group[A] {
  override def quotientRemainder(x: A, y: A) = (multiply(x, inverse(y)), zero)
  override def exactQuotientOption(x: A, y: A) = Some(multiply(x, inverse(y)))
  override def remainder(x: A, y: A) = zero
  def quotientByInt(x: A, y: Int): A = {
    quotientRemainder(x, fromInt(y)) match {
      case (q, r) if r == zero => q
      case _ => throw new ArithmeticException("In the division ring " + this + ", " + x + " is not divisible by " + y)
    }
  }
  def fromRational(x: Fraction[Int]) = quotient(fromInt(x.numerator), fromInt(x.denominator))
  def fromBigRational(x: Fraction[BigInt]) = quotient(fromBigInt(x.numerator), fromBigInt(x.numerator))
}

// there's not much to say here; the only additional requirement to be a field is commutativity, but the type system doesn't see that.
trait Field[@specialized(Float, Double) A] extends DivisionRing[A]

object Field {
  class FieldOfFractions[A: GCDRing] extends Field[Fraction[A]] {
    def ring = implicitly[GCDRing[A]]
    override val one = Fraction.whole(ring.one)
    override val zero = Fraction.whole(ring.zero)
    override def multiply(x: Fraction[A], y: Fraction[A]) = {
      if (x == one) {
        y
      } else if (y == one) {
        x
      } else {
        Fraction(ring.multiply(x.numerator, y.numerator), ring.multiply(x.denominator, y.denominator))
      }
    }
    override def add(x: Fraction[A], y: Fraction[A]) = {
      if (x == zero) {
        y
      } else if (y == zero) {
        x
      } else {
        val denominatorGCD = ring.gcd(x.denominator, y.denominator)
        Fraction(ring.add(ring.multiply(x.numerator, ring.exactQuotient(y.denominator, denominatorGCD)), ring.multiply(ring.exactQuotient(x.denominator, denominatorGCD), y.numerator)), ring.multiply(ring.exactQuotient(x.denominator, denominatorGCD), y.denominator))
      }
    }
    override def fromInt(x: Int) = Fraction.whole(ring.fromInt(x))
    override def fromBigInt(x: BigInt) = Fraction.whole(ring.fromBigInt(x))
    override def negate(x: Fraction[A]) = Fraction.alreadyReduced(ring.negate(x.numerator), x.denominator)
    override def inverse(x: Fraction[A]) = Fraction(x.denominator, x.numerator) // we can't assume it's already reduced, because we might want to move units around
    
    override def toString = "Field.fieldOfFractions(" + ring + ")"
  }

  implicit def fieldOfFractions[A: GCDRing]: Field[Fraction[A]] = new FieldOfFractions[A]

  implicit def forget[A: OrderedField]: Field[A] = implicitly[OrderedField[A]]
}

trait ComplexConjugation[A] { self: Field[A] =>
  def bar(q: A): A
}
