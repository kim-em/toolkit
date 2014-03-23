package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.modules._
import net.tqft.toolkit.algebra.categories._

trait Polynomial[A] extends LinearCombo[A, Int] { polynomial =>
  import net.tqft.toolkit.arithmetic.MinMax._

  override def toString = if (terms.isEmpty) {
    "0"
  } else {
    (terms map { case (g, p) => p.toString + (if (g == 0) "" else " * x^(" + g.toString + ")") }).mkString(" + ")
  }

  def minimumDegree = (terms map { _._1 }).minOption
  def maximumDegree = (terms map { _._1 }).maxOption
  def degree = maximumDegree.get
  def leadingCoefficient = maximumDegree map { coefficientOf(_).get }
  def constantTerm(implicit ring: Ring[A]) = coefficientOf(0).getOrElse(ring.zero)

  def roots(implicit ring: Ring[A] with Elements[A]) = {
    for (x <- ring.elements; if Polynomials.evaluationAt(x).apply(polynomial) == ring.zero) yield x
  }

}

object Polynomial {
  def apply[A: Ring](terms: (Int, A)*) = implicitly[PolynomialAlgebra[A]].wrap(terms.toList)
  def apply[A: Ring](terms: Map[Int, A]) = implicitly[PolynomialAlgebra[A]].wrap(terms)
  implicit def constant[A: Ring](x: A) = apply((0, x))
  implicit def constantFraction[A: EuclideanRing](x: A) = apply((0, Fraction.whole(x)))
  def identity[A: Ring] = apply((1, implicitly[Ring[A]].one))

  def cyclotomic[F: Field](n: Int): Polynomial[F] = {
    val field = implicitly[Field[F]]
    val polynomials = implicitly[PolynomialAlgebraOverField[F]]
    val divisors = for (d <- 1 until n; if n % d == 0) yield cyclotomic(d)
    polynomials.quotient(apply((0, field.negate(field.one)), (n, field.one)), polynomials.product(divisors))
  }

  implicit def coefficientsAsFractions[A: EuclideanRing](polynomial: Polynomial[A]): Polynomial[Fraction[A]] = {
    Polynomial(polynomial.terms.map({ case (i, a) => (i, Fraction.whole(a)) }): _*)
  }

  implicit def over[A: Ring]: PolynomialAlgebra[A] = Polynomials.over(implicitly[Ring[A]])
  implicit def overField[F: Field]: PolynomialAlgebraOverField[F] = Polynomials.over(implicitly[Field[F]])
}

object Polynomials extends HomomorphismCategory[PolynomialAlgebra] {

  val over = new Functor[Ring, Ring, Polynomial] { self =>
    def source = Rings
    def target = Rings

    def apply[A](_ring: Field[A]): PolynomialAlgebraOverField[A] = new PolynomialAlgebraOverField[A] {
      override val ring = _ring
    }

    def apply[A](_ring: Ring[A]): PolynomialAlgebra[A] = new PolynomialAlgebra[A] {
      override val ring = _ring
    }

    def apply[A, B](hom: Homomorphism[Ring, A, B]): Homomorphism[Ring, Polynomial[A], Polynomial[B]] = new RingHomomorphism[Polynomial[A], Polynomial[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(p: Polynomial[A]): Polynomial[B] = new Polynomial[B] {
        def terms = p.terms map { case (k: Int, a) => (k, hom(a)) }
      }
    }
  }

  def evaluationAt[A](x: A)(implicit ring: Ring[A]) = new Homomorphism[Ring, Polynomial[A], A] {
    def source: Ring[Polynomial[A]] = over[A](ring)
    def target: Ring[A] = ring
    def apply(p: Polynomial[A]) = ring.sum(p.terms map { case (e, a) => ring.multiply(a, ring.power(x, e)) })
  }

  val embeddingAsConstants = new NaturalTransformation[Ring, Ring, Functors.Identity, Polynomial] {
    def source = Functors.Identity(Rings)

    def target = over

    def apply[A](o: Ring[A]): Homomorphism[Ring, A, Polynomial[A]] = new Homomorphism[Ring, A, Polynomial[A]] {
      def source = o
      def target = over(o)
      def apply(a: A) = Polynomial((0, a))(o)
    }
  }

}