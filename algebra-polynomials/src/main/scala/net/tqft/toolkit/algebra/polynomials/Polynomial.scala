package net.tqft.toolkit.algebra.polynomials

import scala.language.implicitConversions
import net.tqft.toolkit.algebra._
import scala.collection.immutable.TreeMap

sealed trait Polynomial[A] {
  def mapValues[B:Zero](f: A => B): Polynomial[B]
  def toMap: Map[Int, A]
}

case class MapPolynomial[A:Zero](coefficients: TreeMap[Int, A]) extends Polynomial[A] {
  private def zero = implicitly[Zero[A]].zero
  override def mapValues[B:Zero](f: A => B) = MapPolynomial(TreeMap[Int, B]() ++ coefficients.mapValues(f))
  override def toMap = coefficients.filter(_._2 != zero)
  
  override def equals(other: Any): Boolean = {
    other match {
      case other: Polynomial[A] => toMap == other.toMap
    }
  }
}
case class SeqPolynomial[A:Zero](coefficients: IndexedSeq[A]) extends Polynomial[A] {
  private def zero = implicitly[Zero[A]].zero
  override def mapValues[B:Zero](f: A => B) = SeqPolynomial(coefficients.map(f))
  override def toMap = coefficients.zipWithIndex.collect({ case (x, i) if x != zero => (i, x) }).toMap
  override def equals(other: Any): Boolean = {
    other match {
      case other: SeqPolynomial[A] => coefficients.zipAll(other.coefficients, zero, zero).forall(p => p._1 == p._2)
      case other: MapPolynomial[A] => toMap == other.toMap
    }
  }
}
object SeqPolynomial {
  def apply[A:Zero](terms: A*): Polynomial[A] = {
    val iterms = terms.toIndexedSeq
    val i = iterms.lastIndexWhere(_ != implicitly[Zero[A]].zero)
    SeqPolynomial(iterms.take(i + 1))
  }
}

object Polynomial {
  def apply[A:Zero](terms: (Int, A)*): Polynomial[A] = MapPolynomial(TreeMap[Int, A]() ++ terms)
  def apply[A:Zero](terms: Map[Int, A]): Polynomial[A] = MapPolynomial(TreeMap[Int, A]() ++ terms)
  def apply[A:AdditiveMonoid](coefficients: Seq[A]): Polynomial[A] = SeqPolynomial(coefficients.toIndexedSeq)
  
  def identity[A: Ring] = Polynomial(Map(1 -> implicitly[Ring[A]].one))

  implicit def lift[A:Zero](m: Map[Int, A]): Polynomial[A] = Polynomial(m)
  implicit def liftFractions[A: GCDRing](m: Map[Int, A]): Polynomial[Fraction[A]] = Polynomial(m.mapValues(a => (a: Fraction[A])))
  implicit def liftAsRationalFunction[A: GCDRing](m: Map[Int, A]): Fraction[Polynomial[A]] = lift(m)
  implicit def liftFractionsAsRationalFunction[A: GCDRing](m: Map[Int, A]): Fraction[Polynomial[Fraction[A]]] = liftFractions(m)
  
  implicit def liftSeq[A:AdditiveMonoid](m: Seq[A]): Polynomial[A] = Polynomial(m)
  implicit def liftSeqFractions[A: GCDRing](m: Seq[A]): Polynomial[Fraction[A]] = Polynomial(m.map(a => (a: Fraction[A])))
  implicit def liftSeqAsRationalFunction[A: GCDRing](m: Seq[A]): Fraction[Polynomial[A]] = liftSeq(m)
  implicit def liftSeqFractionsAsRationalFunction[A: GCDRing](m: Seq[A]): Fraction[Polynomial[Fraction[A]]] = liftSeqFractions(m)

  implicit def constant[A:AdditiveMonoid](a: A): Polynomial[A] = Polynomial(Seq(a))
  implicit def constantFraction[A: EuclideanRing](a: A): Polynomial[Fraction[A]] = constant(a)
  implicit def constantRationalFunction[A: GCDRing](a: A): Fraction[Polynomial[A]] = constant(a)
  implicit def constantBigInt(i: Int): Polynomial[BigInt] = constant(i: BigInt)
  implicit def constantFractionBigInt(i: Int): Fraction[Polynomial[BigInt]] = constantRationalFunction(i: BigInt)

  implicit def wholeCoefficients[A: EuclideanRing](p: Polynomial[A]): Polynomial[Fraction[A]] = p.mapValues(x => x: Fraction[A])
  implicit def liftToBigInts(m: Map[Int, Int]): Polynomial[BigInt] = Polynomial(m.mapValues(a => a: BigInt))
  implicit def liftSeqToBigInts(m: Seq[Int]): Polynomial[BigInt] = Polynomial(m.map(a => a: BigInt))
  implicit def liftSeqToBigIntFractions(m: Seq[Int]): Polynomial[Fraction[BigInt]] = Polynomial(m.map(a => (a: BigInt): Fraction[BigInt]))
  implicit def liftSeqFractionToBigIntFractions(m: Seq[Fraction[Int]]): Polynomial[Fraction[BigInt]] = Polynomial(m.map(a => a: Fraction[BigInt]))
  implicit def intCoefficientsToBigInts(p: Polynomial[Int]) = p.mapValues(BigInt.apply)

  implicit def intPolynomialToRationalFunction(p: Polynomial[Int]): RationalFunction[BigInt] = p.mapValues(BigInt.apply)
  
  implicit def polynomialAlgebraAsRing[A: Ring]: Ring[Polynomial[A]] = implicitly[PolynomialAlgebra[A, Polynomial[A]]]
  implicit def polynomialAlgebraAsGCDRing[A: GCDRing]: GCDRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverGCDRing[A, Polynomial[A]]]
  implicit def polynomialAlgebraAsEuclideanRing[A: Field]: EuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverField[A, Polynomial[A]]]
  implicit def polynomialAlgebraOverOrderedFieldAsOrderedEuclideanRing[A: OrderedField]: OrderedEuclideanRing[Polynomial[A]] = implicitly[PolynomialAlgebraOverOrderedField[A, Polynomial[A]]]

  implicit class RichPolynomial[A: Polynomials](p: Polynomial[A]) {
    val polynomials = implicitly[Polynomials[A]]
    def ring = polynomials.ring
    def maximumDegree = polynomials.maximumDegree(p)
    def leadingCoefficient = maximumDegree.map(d => coefficient(d)).getOrElse(polynomials.ring.zero)
    def constantTerm = polynomials.constantTerm(p)
    def coefficient(i: Int) = polynomials.coefficientOf(p)(i)
    def toMap: Map[Int, A] = polynomials.toMap(p)
    def toSeq: IndexedSeq[A] = polynomials.toSeq(p)
    
    def evaluateAt(a: A) = polynomials.evaluateAt(a)(p)
  }

  def cyclotomic[A: Field](n: Int): Polynomial[A] = {
    val ring = implicitly[Field[A]]
    val polynomials = implicitly[PolynomialsOverField[A]]
    val divisors = for (d <- 1 until n; if n % d == 0) yield cyclotomic[A](d)
    polynomials.quotient(Map((0, ring.negate(ring.one)), (n, ring.one)), polynomials.product(divisors))
  }

}


