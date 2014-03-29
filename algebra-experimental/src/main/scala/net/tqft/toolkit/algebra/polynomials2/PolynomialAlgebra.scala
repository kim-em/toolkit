package net.tqft.toolkit.algebra.polynomials2

import scala.language.implicitConversions
import net.tqft.toolkit.algebra._

trait PolynomialAlgebra[A, P] extends Module[A, P] with AssociativeAlgebra[A, P] {
  def toMap(p: P): Map[Int, A]
  def toIndexedSeq[Z: Zero](p: P): Seq[A]
  def fromMap(m: Map[Int, A]): P
  def fromSeq(s: Seq[A]): P
  def monomial(i: Int, a: A) = fromMap(Map(i -> a))

  def maximumDegree(p: P): Option[Int]
  def leadingCoefficient(p: P) = maximumDegree(p).map(coefficientOf(p))
  def coefficientOf(p: P): Int => A
  def evaluateAt(a: A)(p: P): A
  def composeAsFunctions(p1: P, p2: P): P
  def formalDerivative(p: P): P
}
object PolynomialAlgebra {
  class PolynomialAlgebraForMaps[A: Ring] extends PolynomialAlgebra[A, Map[Int, A]] {
    def ring = implicitly[Ring[A]]

    override def toMap(p: Map[Int, A]) = p
    override def toIndexedSeq[Z: Zero](p: Map[Int, A]) = {
      maximumDegree(p) match {
        case None => Seq.empty
        case Some(d) => IndexedSeq.tabulate(d + 1)(i => coefficientOf(p)(i))
      }
    }
    override def fromMap(m: Map[Int, A]) = m
    override def fromSeq(s: Seq[A]) = s.zipWithIndex.collect({ case (a, i) if a != ring.zero => (i, a) }).toMap

    override def add(p1: Map[Int, A], p2: Map[Int, A]) = {
      val newMap = scala.collection.mutable.Map[Int, A]().withDefault(_ => ring.zero)
      for (p <- Seq(p1, p2); (i, a) <- p) {
        newMap(i) = ring.add(newMap(i), a)
      }
      Map() ++ newMap
    }
    override def multiply(p1: Map[Int, A], p2: Map[Int, A]) = ???
    override def negate(p: Map[Int, A]) = p.mapValues(ring.negate)
    override def scalarMultiply(a: A, p: Map[Int, A]) = p.mapValues(x => ring.multiply(a, x))
    override def zero = Map.empty
    override def one = Map(0 -> ring.one)
    override def fromInt(k: Int) = Map(0 -> ring.fromInt(k))

    override def maximumDegree(p: Map[Int, A]) = {
      import net.tqft.toolkit.arithmetic.MinMax._
      p.keys.maxOption
    }
    override def coefficientOf(p: Map[Int, A]) = p
    override def evaluateAt(a: A)(p: Map[Int, A]) = {
      ring.sum(p map { case (e, b) => ring.multiply(b, ring.power(a, e)) })
    }
    override def composeAsFunctions(p1: Map[Int, A], p2: Map[Int, A]) = ???
    override def formalDerivative(p: Map[Int, A]) = ???
  }

  class PolynomialAlgebraForPolynomialWrapper[A: Ring] extends PolynomialAlgebra[A, Polynomial[A]] {
    val implementation = new PolynomialAlgebraForMaps[A]

    override implicit def toMap(p: Polynomial[A]) = p.coefficients
    override def toIndexedSeq[Z: Zero](p: Polynomial[A]) = implementation.toIndexedSeq[Z](p.coefficients)

    override implicit def fromMap(p: Map[Int, A]) = Polynomial(p)
    override def fromSeq(s: Seq[A]) = implementation.fromSeq(s)

    override def add(p1: Polynomial[A], p2: Polynomial[A]) = implementation.add(p1, p2)
    override def multiply(p1: Polynomial[A], p2: Polynomial[A]) = implementation.multiply(p1, p2)
    override def scalarMultiply(a: A, p: Polynomial[A]) = implementation.scalarMultiply(a, p)
    override def negate(p: Polynomial[A]) = implementation.negate(p)

    override def zero = implementation.zero
    override def one = implementation.one
    override def fromInt(k: Int) = implementation.fromInt(k)

    override def maximumDegree(p: Polynomial[A]) = implementation.maximumDegree(p)
    override def coefficientOf(p: Polynomial[A]) = implementation.coefficientOf(p)
    override def evaluateAt(a: A)(p: Polynomial[A]) = implementation.evaluateAt(a)(p)
    override def composeAsFunctions(p1: Polynomial[A], p2: Polynomial[A]) = implementation.composeAsFunctions(p1, p2)
    override def formalDerivative(p: Polynomial[A]) = implementation.formalDerivative(p)
  }

  //  class PolynomialAlgebraForCoefficientSequences[A: Ring] extends PolynomialAlgebra[A, Seq[A]]
  //  class PolynomialAlgebraForCoefficientStreams[A: Ring] extends PolynomialAlgebra[A, Stream[A]]
  //  class PolynomialAlgebraForRootMultiplicities[A: Ring] extends PolynomialAlgebra[A, (A, Map[A, Int])]

  implicit def forMaps[A: Ring]: PolynomialAlgebra[A, Map[Int, A]] = new PolynomialAlgebraForMaps[A]
  implicit def over[A: Ring]: PolynomialAlgebra[A, Polynomial[A]] = new PolynomialAlgebraForPolynomialWrapper[A]
}