package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.modules._
import scala.language.implicitConversions
import net.tqft.toolkit.algebra.modules.LinearCombo

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
  def coefficient(i: Int)(implicit ring: Ring[A]) = coefficientOf(i).getOrElse(ring.zero)
  def constantTerm(implicit ring: Ring[A]) = coefficient(0)
  def evaluateAt(x: A)(implicit ring: Ring[A]) = ring.sum(terms map { case (e, a) => ring.multiply(a, ring.power(x, e)) })
  
  def roots(implicit ring: Ring[A] with Finite[A]) = {
    for (x <- ring.elements; if evaluateAt(x) == ring.zero) yield x
  }

}

object Polynomial {
  def apply[A: Ring](terms: (Int, A)*) = implicitly[PolynomialAlgebra[A]].wrap(terms.toList)
  def apply[A: Ring](terms: Map[Int, A]) = implicitly[PolynomialAlgebra[A]].wrap(terms)
  implicit def constant[A: Ring](x: A): Polynomial[A] = apply((0, x))
  implicit def constantFraction[A: EuclideanRing](x: A): Polynomial[Fraction[A]] = apply((0, Fraction.whole(x)))
  implicit def constantFractionAsRationalFunction[A: EuclideanRing](x: A): Fraction[Polynomial[Fraction[A]]] = {
    val whole_x = Fraction.whole(x)
    val polynomial = apply((0, whole_x))
    
    Fraction.whole(polynomial)(implicitly[PolynomialAlgebraOverField[Fraction[A]]])
  }
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

  implicit def polynomialAlgebraAsRing[A: EuclideanRing]: Ring[Polynomial[A]] = implicitly[PolynomialAlgebra[A]]
  implicit def polynomialAlgebraOverFieldAsEuclideanRing[A: Field]: EuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverField[A]]
  implicit def polynomialAlgebraOverOrderedFieldAsOrderedEuclideanRing[A: OrderedField]: OrderedEuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverOrderedField[A]]

}