package net.tqft.toolkit.algebra

object Fields extends HomomorphismCategory[Field] {
  val embeddingInFieldOfFractions = new NaturalTransformation[EuclideanDomain, EuclideanDomain, Functors.Identity, Fraction] {
    def source = Functors.Identity(EuclideanDomains)

    // TODO why aren't types inferred here?
    def target = fieldOfFractions.andThen[EuclideanDomain, EuclideanDomains.Homomorphism, Functors.Identity](Functors.Forget(Fields, EuclideanDomains))

    def apply[A](o: EuclideanDomain[A]): Homomorphism[EuclideanDomain, A, Fraction[A]] = new Homomorphism[EuclideanDomain, A, Fraction[A]] {
      def source = o
      def target = fieldOfFractions(o)
      def apply(a: A) = Fraction(a, o.one)(o)
    }
  }

  class FieldOfFractions[A](ring: EuclideanDomain[A]) extends Field[Fraction[A]] {
    implicit val _ring = ring
    override val one = Fraction.alreadyReduced(ring.one, ring.one)
    override val zero = Fraction.alreadyReduced(ring.zero, ring.one)
    override def multiply(x: Fraction[A], y: Fraction[A]) = Fraction(ring.multiply(x.numerator, y.numerator), ring.multiply(x.denominator, y.denominator))
    override def add(x: Fraction[A], y: Fraction[A]) = {
      val denominatorGCD = ring.gcd(x.denominator, y.denominator)
      Fraction(ring.add(ring.multiply(x.numerator, ring.quotient(y.denominator, denominatorGCD)), ring.multiply(ring.quotient(x.denominator, denominatorGCD), y.numerator)), ring.multiply(ring.quotient(x.denominator, denominatorGCD), y.denominator))
    }
    override def fromInt(x: Int) = Fraction.alreadyReduced(ring.fromInt(x), ring.one)
    override def negate(x: Fraction[A]) = Fraction.alreadyReduced(ring.negate(x.numerator), x.denominator)
    override def inverse(x: Fraction[A]) = Fraction.alreadyReduced(x.denominator, x.numerator)
  }

  class OrderedFieldOfFractions[A](ring: OrderedEuclideanDomain[A]) extends FieldOfFractions[A](ring) with OrderedField[Fraction[A]] {
    def compare(x: Fraction[A], y: Fraction[A]) = ring.compare(ring.multiply(x.numerator, y.denominator), ring.multiply(y.numerator, x.denominator))
  }

  object Rationals extends OrderedFieldOfFractions(Gadgets.Integers)

  val fieldOfFractions: Functor[EuclideanDomain, Field, Fraction] { def apply[A](ring: OrderedEuclideanDomain[A]): OrderedField[Fraction[A]] } = new Functor[EuclideanDomain, Field, Fraction] { self =>
    def source = EuclideanDomains
    def target = Fields
    def apply[A](ring: EuclideanDomain[A]): Field[Fraction[A]] = new FieldOfFractions(ring)
    def apply[A](ring: OrderedEuclideanDomain[A]): OrderedField[Fraction[A]] = new OrderedFieldOfFractions(ring)
    def apply[A, B](hom: Homomorphism[EuclideanDomain, A, B]): FieldHomomorphism[Fraction[A], Fraction[B]] = new FieldHomomorphism[Fraction[A], Fraction[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(m: Fraction[A]) = Fraction(hom(m.numerator), hom(m.denominator))(hom.target)
    }
  }
}