package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

trait PolynomialAlgebraOverOrderedEuclideanRing[A, P] extends PolynomialAlgebraOverEuclideanRing[A, P] with OrderedEuclideanRing[P] {
  override def ring: OrderedEuclideanRing[A]

  override def compare(p: P, q: P): Int = {
    implicitly[Ordering[Option[Int]]].compare(maximumDegree(p), maximumDegree(q)) match {
      case 0 => {
        if (maximumDegree(p).isEmpty) {
          0
        } else {
          for (i <- maximumDegree(p).get to 0 by -1) {
            ring.compare(coefficientOf(p)(i), coefficientOf(q)(i)) match {
              case 0 => {}
              case c => return c
            }
          }
          0
        }
      }
      case e => e
    }
  }

}

object PolynomialAlgebraOverOrderedEuclideanRing {
  trait PolynomialAlgebraOverOrderedEuclideanRingForMaps[A] extends PolynomialAlgebraOverEuclideanRing.PolynomialAlgebraOverEuclideanRingForMaps[A] with PolynomialAlgebraOverOrderedEuclideanRing[A, Map[Int, A]] {
    override def ring: OrderedEuclideanRing[A]
  }
  trait PolynomialAlgebraOverOrderedEuclideanRingForPolynomials[A] extends PolynomialAlgebraOverEuclideanRing.PolynomialAlgebraOverEuclideanRingForPolynomials[A] with PolynomialAlgebraOverOrderedEuclideanRing[A, Polynomial[A]] {
    override def ring: OrderedEuclideanRing[A]
  }

  implicit def forMaps[A: OrderedEuclideanRing]: PolynomialAlgebraOverOrderedEuclideanRing[A, Map[Int, A]] = new PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverOrderedEuclideanRing[A, Map[Int, A]] {
    override def ring = implicitly[OrderedEuclideanRing[A]]
  }
  implicit def over[A: OrderedEuclideanRing]: PolynomialAlgebraOverOrderedEuclideanRing[A, Polynomial[A]] = new PolynomialAlgebra.PolynomialAlgebraForPolynomials[A] with PolynomialAlgebraOverOrderedEuclideanRing[A, Polynomial[A]] {
    override def ring = implicitly[OrderedEuclideanRing[A]]
  }
}

trait PolynomialsOverOrderedEuclideanRing[A] extends PolynomialAlgebraOverOrderedEuclideanRing[A, Polynomial[A]]
object PolynomialsOverOrderedEuclideanRing {
  implicit def over[A: OrderedEuclideanRing]: PolynomialsOverOrderedEuclideanRing[A] = new PolynomialAlgebraOverOrderedEuclideanRing.PolynomialAlgebraOverOrderedEuclideanRingForPolynomials[A] with PolynomialsOverOrderedEuclideanRing[A] {
    override def ring = implicitly[OrderedEuclideanRing[A]]
  }
}

