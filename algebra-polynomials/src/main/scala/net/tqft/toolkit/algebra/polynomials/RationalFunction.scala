package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.FieldHomomorphism
import net.tqft.toolkit.algebra.Fields
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.categories._
import net.tqft.toolkit.algebra.EuclideanRing


object RationalFunction {
  def identity[A: Field] = {
    val ring = implicitly[Field[A]]
    Fraction(Polynomial.identity, Polynomials.over(ring).one)(Polynomials.over(ring))
  }
}

object RationalFunctions {
  type RationalFunction[A] = Fraction[Polynomial[A]]

  val over = new Functor[Field, Field, RationalFunction] { self =>
    def source = Fields
    def target = Fields

    def apply[A](_field: Field[A]) = Fields.fieldOfFractions(Polynomials.over(_field))

    def apply[A, B](hom: Homomorphism[Field, A, B]): Homomorphism[Field, RationalFunction[A], RationalFunction[B]] = new FieldHomomorphism[RationalFunction[A], RationalFunction[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(f: RationalFunction[A]): RationalFunction[B] = {
        implicit val bPolynomials = Polynomials.over(hom.target)
        val polynomialHom = Polynomials.over(hom)
        Fraction(polynomialHom(f.numerator), polynomialHom(f.denominator))
      }
    }
  }

  def evaluationAt[A](x: A)(implicit field: Field[A]) = new Homomorphism[Field, RationalFunction[A], A] {
    def source: Field[RationalFunction[A]] = over[A](field)
    def target: Field[A] = field
    def apply(p: RationalFunction[A]) = field.quotient(Polynomials.evaluationAt(x).apply(p.numerator), Polynomials.evaluationAt(x).apply(p.denominator))
  }

  val embeddingAsConstants = new NaturalTransformation[Field, Field, Functors.Identity, RationalFunction] {
    def source = Functors.Identity(Fields)

    def target = over

    def apply[A](o: Field[A]): Homomorphism[Field, A, RationalFunction[A]] = new Homomorphism[Field, A, RationalFunction[A]] {
      def source = o
      def target = over(o)
      def apply(a: A) = {
        implicit val polynomials = Polynomials.over(o)
        Fraction(Polynomial((0, a))(o), Polynomial((0, o.one))(o))
      }
    }
  }

}
