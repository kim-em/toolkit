package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

trait PolynomialAlgebraOverOrderedField[A, P] extends PolynomialAlgebraOverField[A, P] with OrderedEuclideanRing[P] {
  override def ring: OrderedField[A]

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

object PolynomialAlgebraOverOrderedField {
  trait PolynomialAlgebraOverOrderedFieldForMaps[A] extends PolynomialAlgebraOverField.PolynomialAlgebraOverFieldForMaps[A] with PolynomialAlgebraOverOrderedField[A, Map[Int, A]] {
    override def ring: OrderedField[A]
  }
  implicit def forMaps[A: OrderedField]: PolynomialAlgebraOverOrderedField[A, Map[Int, A]] = new PolynomialAlgebraOverField.PolynomialAlgebraOverFieldForMaps[A] with PolynomialAlgebraOverOrderedField[A, Map[Int, A]] {
    override def ring = implicitly[OrderedField[A]]
  }
  implicit def over[A: OrderedField]: PolynomialAlgebraOverOrderedField[A, Polynomial[A]] = PolynomialsOverOrderedField.over[A]
}

abstract class PolynomialsOverOrderedField[A:OrderedField] extends PolynomialsOverField[A] with PolynomialAlgebraOverOrderedField[A, Polynomial[A]]
object PolynomialsOverOrderedField {
  implicit def over[A: OrderedField]: PolynomialsOverOrderedField[A] = new PolynomialsOverOrderedField[A] { 
    override def ring: OrderedField[A] = implicitly[OrderedField[A]]
  }
}

