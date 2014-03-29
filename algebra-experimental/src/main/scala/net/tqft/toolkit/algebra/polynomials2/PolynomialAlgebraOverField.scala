package net.tqft.toolkit.algebra.polynomials2

import net.tqft.toolkit.algebra._

trait PolynomialAlgebraOverField[A, P] extends PolynomialAlgebra[A, P] with EuclideanRing[P] {
  def ring: Field[A]

  def quotientRemainder(x: P, y: P): (P, P) = {
    (maximumDegree(x), maximumDegree(y)) match {
      case (_, None) => throw new ArithmeticException
      case (None, Some(dy)) => (zero, zero)
      case (Some(dx), Some(dy)) => {
        if (dy > dx) {
          (zero, x)
        } else {
          val ax = leadingCoefficient(x).get
          val ay = leadingCoefficient(y).get

          require(ax != ring.zero)
          require(ay != ring.zero)

          val q = ring.quotient(ax, ay)

          val quotientLeadingTerm = monomial(dx - dy, q)
          val difference = add(x, negate(multiply(quotientLeadingTerm, y)))
          //          require(difference.coefficientOf(dx) == None)
          val (restOfQuotient, remainder) = quotientRemainder(difference, y)

          (add(quotientLeadingTerm, restOfQuotient), remainder)
        }
      }
    }
  }

  def removeMultipleRoots(p: P): P = {
    quotient(p, gcd(p, formalDerivative(p)))
  }
}

object PolynomialAlgebraOverField {
  implicit def forMaps[A: Field]: PolynomialAlgebraOverField[A, Map[Int, A]] = new PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverField[A, Map[Int, A]] {
    override def ring = implicitly[Field[A]]
  }
  implicit def over[A: Field]: PolynomialAlgebraOverField[A, Polynomial[A]] = new PolynomialAlgebra.PolynomialAlgebraForPolynomialWrapper[A]with PolynomialAlgebraOverField[A, Polynomial[A]] {
    override def ring = implicitly[Field[A]]
  }
}
