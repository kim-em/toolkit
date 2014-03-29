package net.tqft.toolkit.algebra.polynomials

import scala.language.implicitConversions

import net.tqft.toolkit.algebra._

case class Polynomial[A](coefficients: Map[Int, A])

object Polynomial {
  def apply[A](terms: (Int, A)*): Polynomial[A] = Polynomial(terms.toMap)

  def identity[A: Ring] = Polynomial(Map(1 -> implicitly[Ring[A]].one))
  
  implicit def constant[A](a: A): Polynomial[A] = Polynomial(Map(0 -> a))
  implicit def constantFraction[A: EuclideanRing](a: A): Polynomial[Fraction[A]] = constant(a)
  implicit def constantRationalFunction[A: Field](a: A): Fraction[Polynomial[A]] = constant(a)
  implicit def constantFractionRationalFunction[A: OrderedEuclideanRing](a: A): Fraction[Polynomial[Fraction[A]]] = constantRationalFunction[Fraction[A]](a)

  implicit def wholeCoefficients[A: EuclideanRing](p: Polynomial[A]): Polynomial[Fraction[A]] = {
    Polynomial(p.coefficients.mapValues(x => x: Fraction[A]))
  }
  
  implicit def polynomialAlgebraAsRing[A: EuclideanRing]: Ring[Polynomial[A]] = implicitly[PolynomialAlgebra[A, Polynomial[A]]]
  implicit def polynomialAlgebraOverFieldAsEuclideanRing[A: Field]: EuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverField[A, Polynomial[A]]]
  implicit def polynomialAlgebraOverOrderedFieldAsOrderedEuclideanRing[A: OrderedField]: OrderedEuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverOrderedField[A, Polynomial[A]]]

  implicit class RichPolynomial[A: Polynomials](p: Polynomial[A]) {
	  val polynomials = implicitly[Polynomials[A]]
	  def ring = polynomials.ring
	  def maximumDegree = polynomials.maximumDegree(p)
	  def constantTerm = polynomials.constantTerm(p)
	  def coefficient(i: Int) = polynomials.coefficientOf(p)(i)
	  def toMap: Map[Int, A] = polynomials.toMap(p)
	  def toIndexedSeq: IndexedSeq[A] = polynomials.toIndexedSeq(p)(ring)
  }
  
  def cyclotomic[F: Field](n: Int): Polynomial[F] = {
    val field = implicitly[Field[F]]
    val polynomials = implicitly[PolynomialsOverField[F]]
    val divisors = for (d <- 1 until n; if n % d == 0) yield cyclotomic(d)
    polynomials.quotient(apply((0, field.negate(field.one)), (n, field.one)), polynomials.product(divisors))
  }

}


