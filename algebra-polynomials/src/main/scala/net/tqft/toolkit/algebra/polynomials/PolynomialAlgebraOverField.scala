package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.EuclideanRing
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.OrderedEuclideanRing
import net.tqft.toolkit.algebra.OrderedField

trait PolynomialAlgebraOverField[A] extends PolynomialAlgebra[A] with EuclideanRing[Polynomial[A]] {
  override def ring: Field[A]

  def quotientRemainder(x: Polynomial[A], y: Polynomial[A]): (Polynomial[A], Polynomial[A]) = {
    (x.maximumDegree, y.maximumDegree) match {
      case (_, None) => throw new ArithmeticException
      case (None, Some(dy)) => (zero, zero)
      // FIXME why are these type ascriptions needed?
      case (Some(dx: Int), Some(dy: Int)) => {
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
  implicit def overField[F: Field]: PolynomialAlgebraOverField[F] = Polynomials.over(implicitly[Field[F]])
}

trait PolynomialAlgebraOverOrderedField[A] extends PolynomialAlgebraOverField[A] with OrderedEuclideanRing[Polynomial[A]] {
  override def ring: OrderedField[A]
  
  override def compare(p: Polynomial[A], q: Polynomial[A]): Int = {
    implicitly[Ordering[Option[Int]]].compare(p.maximumDegree, q.maximumDegree)  match {
      case 0 => {
        if(p.maximumDegree.isEmpty) {
          0
        } else {
          for(i <- p.maximumDegree.get to 0 by -1) {
            ring.compare(p.coefficient(i)(ring), q.coefficient(i)(ring)) match {
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
  implicit def overOrderedField[F: OrderedField]: PolynomialAlgebraOverOrderedField[F] = Polynomials.over(implicitly[OrderedField[F]])
}

