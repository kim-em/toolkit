package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

trait PolynomialAlgebraOverField[A, P] extends PolynomialAlgebra[A, P] with EuclideanRing[P] {
  override def ring: Field[A]

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
  trait PolynomialAlgebraOverFieldForMaps[A] extends PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverField[A, Map[Int, A]] {
    override def ring: Field[A]
  }
  trait PolynomialAlgebraOverFieldForPolynomials[A] extends PolynomialAlgebra.PolynomialAlgebraForPolynomials[A] with PolynomialAlgebraOverField[A, Polynomial[A]] {
    override def ring: Field[A]
  }
  
  implicit def forMaps[A: Field]: PolynomialAlgebraOverField[A, Map[Int, A]] = new PolynomialAlgebraOverFieldForMaps[A] {
    override def ring = implicitly[Field[A]]
  }
  implicit def over[A: Field]: PolynomialAlgebraOverField[A, Polynomial[A]] = new PolynomialAlgebra.PolynomialAlgebraForPolynomials[A] with PolynomialAlgebraOverField[A, Polynomial[A]] {
    override def ring = implicitly[Field[A]]
  }
}

trait PolynomialsOverField[A] extends PolynomialAlgebraOverField[A, Polynomial[A]] with Polynomials[A]
object PolynomialsOverField {
  implicit def over[A: Field]: PolynomialsOverField[A] = new PolynomialAlgebraOverField.PolynomialAlgebraOverFieldForPolynomials[A] with PolynomialsOverField[A] {
    override def ring = implicitly[Field[A]]
  }
}

