package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.EuclideanRing
import net.tqft.toolkit.algebra.Field

trait PolynomialAlgebraOverField[A] extends PolynomialAlgebra[A] with EuclideanRing[Polynomial[A]] {
  override implicit def ring: Field[A]

  def quotientRemainder(x: Polynomial[A], y: Polynomial[A]): (Polynomial[A], Polynomial[A]) = {
    (x.maximumDegree, y.maximumDegree) match {
      case (_, None) => throw new ArithmeticException
      case (None, Some(dy)) => (zero, zero)
      case (Some(dx), Some(dy)) => {
        if (dy > dx) {
          (zero, x)
        } else {
          val ax = x.leadingCoefficient.get
          val ay = y.leadingCoefficient.get

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

  def removeMultipleRoots(p: Polynomial[A]): Polynomial[A] = {
    quotient(p, gcd(p, formalDerivative(p)))
  }
}

object PolynomialAlgebraOverField {
  implicit def over[A:Field] = Polynomials.over(implicitly[Field[A]])
}
