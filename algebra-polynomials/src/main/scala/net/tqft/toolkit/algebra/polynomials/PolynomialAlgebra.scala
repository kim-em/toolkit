package net.tqft.toolkit.algebra.polynomials

import scala.language.implicitConversions

import net.tqft.toolkit.algebra._

trait PolynomialAlgebra[A, P] extends Module[A, P] with AssociativeAlgebra[A, P] {
  def ring: Ring[A]

  def toMap(p: P): Map[Int, A]
  def toSeq(p: P): IndexedSeq[A]
  def fromMap(m: Map[Int, A]): P
  def fromSeq(s: Seq[A]): P
  
  def monomial(i: Int, a: A = ring.one) = fromMap(new scala.collection.immutable.Map.Map1(i, a))
  def constant(a: A) = monomial(0, a)
  override lazy val zero = fromMap(Map.empty)
  override lazy val one = monomial(0, ring.one)
  override def fromInt(k: Int) = constant(ring.fromInt(k))
  override def fromBigInt(k: BigInt) = constant(ring.fromBigInt(k))
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
    override def toSeq(p: Map[Int, A]) = {
      maximumDegree(p) match {
        case None => IndexedSeq.empty[A]
        case Some(d) => IndexedSeq.tabulate(d + 1)(i => coefficientOf(p)(i))
      }
    }
    override def fromMap(m: Map[Int, A]) = m
    override def fromSeq(s: Seq[A]) = s.zipWithIndex.collect({ case (a, i) if !ring.zero_?(a) => (i, a) }).toMap

    override def zero_?(p: Map[Int, A]) = {
      if (p.isEmpty) {
        true
      } else {
//        require(p.valuesIterator.forall(x => !coefficients.zero_?(x)))
        false
      }
    }

    override def maximumDegree(p: Map[Int, A]) = {
      import net.tqft.toolkit.arithmetic.MinMax._
      p.keys.maxOption
    }
    override def coefficientOf(p: Map[Int, A]) = { i => p.getOrElse(i, ring.zero) }
    override def evaluateAt(a: A)(p: Map[Int, A]) = {
      ring.sum(p map { case (e, b) => ring.multiply(b, ring.power(a, e)) })
    }
    override def composeAsFunctions(p1: Map[Int, A], p2: Map[Int, A]) = ???
    override def formalDerivative(p: Map[Int, A]) = p.collect({
      case (i, a) if i != 0 => (i - 1, multiplicativeCoefficients.multiply(multiplicativeCoefficients.fromInt(i), a))
    })
  }

  abstract class PolynomialAlgebraForCoefficientSequences[A: Ring] extends Ring.RingSeq[A] with PolynomialAlgebra[A, Seq[A]] {
    override def toMap(p: Seq[A]): Map[Int, A] = ???
    override def toSeq(p: Seq[A]) = p.toIndexedSeq

    override def fromMap(p: Map[Int, A]): Seq[A] = {
      val keys = p.keySet
      if (keys.isEmpty) {
        IndexedSeq.empty
      } else {
        IndexedSeq.tabulate(keys.max + 1)({ i => p.getOrElse(i, ring.zero) })//.ensuring(s => toMap(s) == p)
      }
    }
    override def fromSeq(s: Seq[A]) = s

    override def zero_?(s: Seq[A]): Boolean = {
      if (s.isEmpty) {
        true
      } else {
        // TODO remove
//        require(!coefficients.zero_?(s.last))
        false
      }
    }

    override def maximumDegree(p: Seq[A]) = ???
    override def coefficientOf(p: Seq[A]) = { i => if(i < p.size) p(i) else ring.zero }
    override def evaluateAt(a: A)(p: Seq[A]) = ring.sum(for((x,i) <- p.zipWithIndex) yield ring.multiply(x, ring.power(a, i)))
    override def composeAsFunctions(p1: Seq[A], p2: Seq[A]) = {
      sum(p2.zipWithIndex.map(p => scalarMultiply(p._1, power(p1, p._2))))
    }
    override def formalDerivative(p: Seq[A]) = p.zipWithIndex.tail.map(p => ring.multiply(p._1, ring.fromInt(p._2)))
  }
  //  class PolynomialAlgebraForCoefficientStreams[A: Ring] extends Ring.RingMap[Int, A] with PolynomialAlgebra[A, Stream[A]] 
  //  class PolynomialAlgebraForRootMultiplicities[A: Ring] extends PolynomialAlgebra[A, (A, Map[A, Int])]

  implicit def forMaps[A: Ring]: PolynomialAlgebra[A, Map[Int, A]] = new PolynomialAlgebraForMaps[A] {
    override def ring = implicitly[Ring[A]]
  }
  implicit def over[A: Ring]: PolynomialAlgebra[A, Polynomial[A]] = new Polynomials[A] {
    override def ring = implicitly[Ring[A]]
  }
}

abstract class Polynomials[A: Ring] extends PolynomialAlgebra[A, Polynomial[A]] {
  private val mapImplementation = new PolynomialAlgebra.PolynomialAlgebraForMaps[A] {
    override def ring = implicitly[Ring[A]]
  }
  private val seqImplementation = new PolynomialAlgebra.PolynomialAlgebraForCoefficientSequences[A] {
    override def ring = implicitly[Ring[A]]
  }

  override def toMap(p: Polynomial[A]) = p.toMap
  override def fromMap(m: Map[Int, A]) = Polynomial(m)
  override def maximumDegree(p: Polynomial[A]) = {
    p match {
      case p: MapPolynomial[A] => p.coefficients.keySet.lastOption.ensuring(k => k.isEmpty || !ring.zero_?(coefficientOf(p)(k.get)))
      case p: SeqPolynomial[A] => p.coefficients.size match {
        case 0 => None
        case k => {
          require(!ring.zero_?(p.coefficients.last))
          Some(k - 1)
        }
      }
    }
  }
  override def leadingCoefficient(p: Polynomial[A]) = p match {
    case p: MapPolynomial[A] => p.coefficients.lastOption.map(_._2)
    case p: SeqPolynomial[A] => {
      p.coefficients.lastIndexWhere({ a: A => !ring.zero_?(a) }) match {
        case -1 => None
        case k => Some(p.coefficients(k))
      }
    }
  }

  override def fromSeq(s: Seq[A]): Polynomial[A] = SeqPolynomial(s.toIndexedSeq)
  def toSeq(p: Polynomial[A]): IndexedSeq[A] = {
    p match {
      case p: MapPolynomial[A] => maximumDegree(p) match {
        case None => IndexedSeq.empty
        case Some(k) => IndexedSeq.tabulate(k + 1)({ i => p.coefficients.getOrElse(i, ring.zero) })
      }
      case p: SeqPolynomial[A] => p.coefficients
    }
  }

  override def zero_?(p: Polynomial[A]): Boolean = p match {
    case p: MapPolynomial[A] => mapImplementation.zero_?(p.coefficients)
    case p: SeqPolynomial[A] => seqImplementation.zero_?(p.coefficients)
  }

  override def monomial(i: Int, a: A = ring.one) = {
    if (i < 10) {
      fromSeq(IndexedSeq.fill(i)(ring.zero) :+ a)
    } else {
      fromMap(new scala.collection.immutable.Map.Map1(i, a))
    }
  }
  override lazy val zero = fromSeq(Seq.empty)

  override def add(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {
    (x, y) match {
      case (x: SeqPolynomial[A], y: SeqPolynomial[A]) => {
        seqImplementation.add(x.coefficients, y.coefficients)
      }
      case (x, y) => {
        mapImplementation.add(x.toMap, y.toMap)
      }
    }
  }
  override def scalarMultiply(a: A, b: Polynomial[A]): Polynomial[A] = b.mapValues(x => ring.multiply(a, x))
  override def multiply(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {
    (x, y) match {
      case (x: SeqPolynomial[A], y: SeqPolynomial[A]) => {
        seqImplementation.multiply(x.coefficients, y.coefficients)
      }
      case (x, y) => {
        mapImplementation.multiply(x.toMap, y.toMap)
      }
    }
  }
  override def negate(x: Polynomial[A]): Polynomial[A] = x.mapValues(y => ring.negate(y))

  def coefficientOf(p: Polynomial[A]): Int => A = {
    p match {
      case p: MapPolynomial[A] => p.coefficients.lift.andThen(_.getOrElse(ring.zero))
      case p: SeqPolynomial[A] => { i => if(i < p.coefficients.size) p.coefficients(i) else ring.zero }
    }
  }

  def composeAsFunctions(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {
    (x, y) match {
      case (x: SeqPolynomial[A], y: SeqPolynomial[A]) => {
        seqImplementation.composeAsFunctions(x.coefficients, y.coefficients)
      }
      case (x, y) => {
        mapImplementation.composeAsFunctions(x.toMap, y.toMap)
      }
    }
  }
  def evaluateAt(a: A)(p: Polynomial[A]): A = {
    p match {
      case p: MapPolynomial[A] => mapImplementation.evaluateAt(a)(p.coefficients)
      case p: SeqPolynomial[A] => seqImplementation.evaluateAt(a)(p.coefficients)
    }
  }
  def formalDerivative(p: Polynomial[A]): Polynomial[A] = {
    p match {
      case p: MapPolynomial[A] => mapImplementation.formalDerivative(p.coefficients)
      case p: SeqPolynomial[A] => seqImplementation.formalDerivative(p.coefficients)
    }
  }
  
    override def toString = s"Polynomials.over($ring)"

}

object Polynomials {
  implicit def over[A: Ring]: Polynomials[A] = new Polynomials[A] {
    override def ring = implicitly[Ring[A]]
  }
}

