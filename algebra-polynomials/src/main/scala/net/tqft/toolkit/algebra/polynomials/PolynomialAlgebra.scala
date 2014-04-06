package net.tqft.toolkit.algebra.polynomials

import scala.language.implicitConversions

import net.tqft.toolkit.algebra._

trait PolynomialAlgebra[A, P] extends Module[A, P] with AssociativeAlgebra[A, P] {
  def ring: Ring[A]

  def toMap(p: P): Map[Int, A]
  def toIndexedSeq[Z: Zero](p: P): IndexedSeq[A]
  def fromMap(m: Map[Int, A]): P
  def fromSeq(s: Seq[A]): P

  def monomial(i: Int, a: A = ring.one) = fromMap(Map(i -> a))
  def constant(a: A) = monomial(0, a)
  override def zero = fromMap(Map.empty)
  override def one = monomial(0, ring.one)
  override def fromInt(k: Int) = constant(ring.fromInt(k))
  def identity = monomial(1, ring.one)

  def maximumDegree(p: P): Option[Int]
  def leadingCoefficient(p: P) = maximumDegree(p).map(coefficientOf(p))
  def constantTerm(p: P) = coefficientOf(p)(0)
  def coefficientOf(p: P): Int => A
  def evaluateAt(a: A)(p: P): A
  
  def composeAsFunctions(p1: P, p2: P): P
  
  def formalDerivative(p: P): P
}

object PolynomialAlgebra {
  abstract class PolynomialAlgebraForMaps[A: Ring] extends Ring.RingMap[Int, A] with PolynomialAlgebra[A, Map[Int, A]] {
    override def keys = implicitly[AdditiveMonoid[Int]]
    override def multiplicativeCoefficients = implicitly[Ring[A]]
    override def coefficients = implicitly[Module[A, A]]
    
    override def toMap(p: Map[Int, A]) = p
    override def toIndexedSeq[Z: Zero](p: Map[Int, A]) = {
      maximumDegree(p) match {
        case None => IndexedSeq.empty[A]
        case Some(d) => IndexedSeq.tabulate(d + 1)(i => coefficientOf(p)(i))
      }
    }
    override def fromMap(m: Map[Int, A]) = m
    override def fromSeq(s: Seq[A]) = s.zipWithIndex.collect({ case (a, i) if a != ring.zero => (i, a) }).toMap

    override def maximumDegree(p: Map[Int, A]) = {
      import net.tqft.toolkit.arithmetic.MinMax._
      p.keys.maxOption
    }
    override def coefficientOf(p: Map[Int, A]) = p
    override def evaluateAt(a: A)(p: Map[Int, A]) = {
      ring.sum(p map { case (e, b) => ring.multiply(b, ring.power(a, e)) })
    }
    override def composeAsFunctions(p1: Map[Int, A], p2: Map[Int, A]) = ???
    override def formalDerivative(p: Map[Int, A]) = p.collect({
      case (i, a) if i != 0 => (i - 1, multiplicativeCoefficients.multiply(multiplicativeCoefficients.fromInt(i), a))
    })
  }

  abstract class PolynomialAlgebraForWrapper[A: Ring, P] extends PolynomialAlgebra[A, P] {
    val implementation = new PolynomialAlgebraForMaps[A] {
      override def ring = implicitly[Ring[A]]
    }

    override implicit def toMap(p: P): Map[Int, A]
    override def toIndexedSeq[Z: Zero](p: P) = implementation.toIndexedSeq[Z](toMap(p))

    override implicit def fromMap(p: Map[Int, A]): P
    override def fromSeq(s: Seq[A]) = implementation.fromSeq(s)

    override def add(p1: P, p2: P) = implementation.add(p1, p2)
    override def multiply(p1: P, p2: P) = implementation.multiply(p1, p2)
    override def scalarMultiply(a: A, p: P) = implementation.scalarMultiply(a, p)
    override def negate(p: P) = implementation.negate(p)

    override def maximumDegree(p: P) = implementation.maximumDegree(p)
    override def coefficientOf(p: P) = implementation.coefficientOf(p)
    override def evaluateAt(a: A)(p: P) = implementation.evaluateAt(a)(p)
    override def composeAsFunctions(p1: P, p2: P) = implementation.composeAsFunctions(p1, p2)
    override def formalDerivative(p: P) = implementation.formalDerivative(p)
  }

  abstract class PolynomialAlgebraForPolynomials[A: Ring] extends PolynomialAlgebraForWrapper[A, Polynomial[A]] {

    override def toMap(p: Polynomial[A]) = p.coefficients
    override def fromMap(m: Map[Int, A]) = Polynomial(m)
  }

  //  class PolynomialAlgebraForCoefficientSequences[A: Ring] extends PolynomialAlgebra[A, Seq[A]]
  //  class PolynomialAlgebraForCoefficientStreams[A: Ring] extends Ring.RingMap[Int, A] with PolynomialAlgebra[A, Stream[A]] 
  //  class PolynomialAlgebraForRootMultiplicities[A: Ring] extends PolynomialAlgebra[A, (A, Map[A, Int])]

  implicit def forMaps[A: Ring]: PolynomialAlgebra[A, Map[Int, A]] = new PolynomialAlgebraForMaps[A] {
    override def ring = implicitly[Ring[A]]
  }
  implicit def over[A: Ring]: PolynomialAlgebra[A, Polynomial[A]] = new PolynomialAlgebraForPolynomials[A] {
    override def ring = implicitly[Ring[A]]
  }
}

trait Polynomials[A] extends PolynomialAlgebra[A, Polynomial[A]]

object Polynomials {
  implicit def over[A: Ring]: Polynomials[A] = new PolynomialAlgebra.PolynomialAlgebraForPolynomials[A] with Polynomials[A] {
    override def ring = implicitly[Ring[A]]
  }
}

