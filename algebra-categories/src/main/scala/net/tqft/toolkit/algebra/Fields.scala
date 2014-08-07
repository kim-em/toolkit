package net.tqft.toolkit.algebra

import scala.language.reflectiveCalls

import net.tqft.toolkit.algebra.categories._

trait FieldHomomorphism[A, B] extends Homomorphism[Field, A, B]

object Fields extends HomomorphismCategory[Field] {
  val embeddingInFieldOfFractions = new NaturalTransformation[EuclideanRing, EuclideanRing, Functors.Identity, Fraction] {
    def source = Functors.Identity(EuclideanRings)

    def target = fieldOfFractions.andThen[EuclideanRing, EuclideanRings.Homomorphism, Functors.Identity](Functors.Forget(Fields, EuclideanRings))

    def apply[A](o: EuclideanRing[A]): Homomorphism[EuclideanRing, A, Fraction[A]] = new Homomorphism[EuclideanRing, A, Fraction[A]] {
      def source = o
      def target = fieldOfFractions(o)
      def apply(a: A) = Fraction(a, o.one)(o)
    }
  }

  val fieldOfFractions: Functor[EuclideanRing, Field, Fraction] { def apply[A](ring: OrderedEuclideanRing[A]): OrderedField[Fraction[A]] } = new Functor[EuclideanRing, Field, Fraction] { self =>
    def source = EuclideanRings
    def target = Fields
    def apply[A](ring: EuclideanRing[A]): Field[Fraction[A]] = new Field.FieldOfFractions[A]()(ring)
    def apply[A](ring: OrderedEuclideanRing[A]): OrderedField[Fraction[A]] = new OrderedField.OrderedFieldOfFractions[A]()(ring)
    def apply[A, B](hom: Homomorphism[EuclideanRing, A, B]): FieldHomomorphism[Fraction[A], Fraction[B]] = new FieldHomomorphism[Fraction[A], Fraction[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(m: Fraction[A]) = Fraction(hom(m.numerator), hom(m.denominator))(hom.target)
    }
  }
}

