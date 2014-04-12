package net.tqft.toolkit.algebra.polynomials

import scala.language.implicitConversions

import net.tqft.toolkit.algebra._

case class Polynomial[A](coefficients: Map[Int, A]) {
//  require(coefficients.valuesIterator.forall(_ != implicitly[Rig[A]].zero))
  def degree = {
    import net.tqft.toolkit.arithmetic.MinMax._
    coefficients.keySet.maxOption
  }
  def mapValues[B](f: A => B) = Polynomial(coefficients.mapValues(f))
}

object Polynomial {
  def apply[A:Rig](terms: (Int, A)*): Polynomial[A] = Polynomial(terms.toMap)

  def identity[A: Ring] = Polynomial(Map(1 -> implicitly[Ring[A]].one))
  
  implicit def lift[A](m: Map[Int, A]): Polynomial[A] = Polynomial(m)
  implicit def liftFractions[A: GCDRing](m: Map[Int, A]): Polynomial[Fraction[A]] = Polynomial(m.mapValues(a => (a: Fraction[A])))
  
  implicit def constant[A](a: A): Polynomial[A] = Polynomial(Map(0 -> a))
  implicit def constantFraction[A: EuclideanRing](a: A): Polynomial[Fraction[A]] = constant(a)
  implicit def constantRationalFunction[A: Field](a: A): Fraction[Polynomial[A]] = constant(a)
  implicit def constantFractionRationalFunction[A: OrderedEuclideanRing](a: A): Fraction[Polynomial[Fraction[A]]] = constantRationalFunction[Fraction[A]](a)

  implicit def wholeCoefficients[A: EuclideanRing](p: Polynomial[A]): Polynomial[Fraction[A]] = {
    Polynomial(p.coefficients.mapValues(x => x: Fraction[A]))
  }
  
  implicit def polynomialAlgebraAsRing[A: Ring]: Ring[Polynomial[A]] = implicitly[PolynomialAlgebra[A, Polynomial[A]]]
  implicit def polynomialAlgebraAsEuclideanRing[A: EuclideanRing]: EuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverEuclideanRing[A, Polynomial[A]]]
  implicit def polynomialAlgebraOverOrderedEuclideanRingAsOrderedEuclideanRing[A: OrderedEuclideanRing]: OrderedEuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverOrderedEuclideanRing[A, Polynomial[A]]]

  implicit class RichPolynomial[A: Polynomials](p: Polynomial[A]) {
	  val polynomials = implicitly[Polynomials[A]]
	  def ring = polynomials.ring
	  def maximumDegree = polynomials.maximumDegree(p)
	  def constantTerm = polynomials.constantTerm(p)
	  def coefficient(i: Int) = polynomials.coefficientOf(p)(i)
	  def toMap: Map[Int, A] = polynomials.toMap(p)
	  def toIndexedSeq: IndexedSeq[A] = polynomials.toIndexedSeq(p)(ring)
  }
  
  def cyclotomic[A: EuclideanRing](n: Int): Polynomial[A] = {
    val ring = implicitly[EuclideanRing[A]]
    val polynomials = implicitly[PolynomialsOverEuclideanRing[A]]
    val divisors = for (d <- 1 until n; if n % d == 0) yield cyclotomic[A](d)
    polynomials.quotient(apply((0, ring.negate(ring.one)), (n, ring.one)), polynomials.product(divisors))
  }

}


