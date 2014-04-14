package net.tqft.toolkit.algebra.polynomials

import scala.language.implicitConversions
import net.tqft.toolkit.algebra._
import scala.collection.immutable.TreeMap

case class Polynomial[A](coefficients: TreeMap[Int, A]) {
  def degree = coefficients.keySet.lastOption
  def mapValues[B](f: A => B) = Polynomial(TreeMap[Int, B]() ++ coefficients.mapValues(f))
}

object Polynomial {
  def apply[A](terms: (Int, A)*): Polynomial[A] = Polynomial(TreeMap[Int, A]() ++ terms)
  def apply[A](terms: Map[Int, A]): Polynomial[A] = Polynomial(TreeMap[Int, A]() ++ terms)

  def identity[A: Ring] = Polynomial(Map(1 -> implicitly[Ring[A]].one))
  
  implicit def lift[A](m: Map[Int, A]): Polynomial[A] = Polynomial(m)
  implicit def liftFractions[A: GCDRing](m: Map[Int, A]): Polynomial[Fraction[A]] = Polynomial(m.mapValues(a => (a: Fraction[A])))
  implicit def liftAsRationalFunction[A: GCDRing](m: Map[Int, A]): Fraction[Polynomial[A]] = lift(m)
  implicit def liftFractionsAsRationalFunction[A: GCDRing](m: Map[Int, A]): Fraction[Polynomial[Fraction[A]]] = liftFractions(m)
  
  implicit def constant[A](a: A): Polynomial[A] = Polynomial(Map(0 -> a))
  implicit def constantFraction[A: EuclideanRing](a: A): Polynomial[Fraction[A]] = constant(a)
  implicit def constantRationalFunction[A: Field](a: A): Fraction[Polynomial[A]] = constant(a)
  implicit def constantFractionRationalFunction[A: OrderedEuclideanRing](a: A): Fraction[Polynomial[Fraction[A]]] = constantRationalFunction[Fraction[A]](a)

  implicit def wholeCoefficients[A: EuclideanRing](p: Polynomial[A]): Polynomial[Fraction[A]] = {
    Polynomial(p.coefficients.mapValues(x => x: Fraction[A]))
  }
  implicit def liftToBigInts(m: Map[Int, Int]): Polynomial[BigInt] = Polynomial(m.mapValues(a => a: BigInt))
  implicit def intCoefficientsToBigInts(p: Polynomial[Int]) = p.mapValues(BigInt.apply)
  
  implicit def polynomialAlgebraAsRing[A: Ring]: Ring[Polynomial[A]] = implicitly[PolynomialAlgebra[A, Polynomial[A]]]
  implicit def polynomialAlgebraAsGCDRing[A: GCDRing]: GCDRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverGCDRing[A, Polynomial[A]]]
  implicit def polynomialAlgebraAsEuclideanRing[A: Field]: EuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverField[A, Polynomial[A]]]
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
  
  def cyclotomic[A: Field](n: Int): Polynomial[A] = {
    val ring = implicitly[Field[A]]
    val polynomials = implicitly[PolynomialsOverField[A]]
    val divisors = for (d <- 1 until n; if n % d == 0) yield cyclotomic[A](d)
    polynomials.quotient(apply((0, ring.negate(ring.one)), (n, ring.one)), polynomials.product(divisors))
  }

}


