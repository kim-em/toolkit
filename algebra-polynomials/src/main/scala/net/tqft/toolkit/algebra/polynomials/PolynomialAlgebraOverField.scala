package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.EuclideanRing
import net.tqft.toolkit.algebra.GCDRing
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.Field

trait PolynomialAlgebraOverField[A, P] extends PolynomialAlgebraOverGCDRing[A, P] with EuclideanRing[P] {
  override def ring: Field[A]

  override def quotientRemainder(x: P, y: P): (P, P) = {
    println(s"beginning quotientRemainder($x, $y)")
    (maximumDegree(x), maximumDegree(y)) match {
      case (_, None) => throw new ArithmeticException
      case (None, Some(dy)) => (zero, zero)
      case (Some(dx), Some(dy)) => {
        (if (dy > dx) {
          (zero, x)
        } else {
          val ax = leadingCoefficient(x).get
          val ay = leadingCoefficient(y).get

          require(ax != ring.zero)
          require(ay != ring.zero)

          val q = ring.quotient(ax, ay)

          val quotientLeadingTerm = monomial(dx - dy, q)
          val difference = add(x, negate(multiply(quotientLeadingTerm, y)))
          require(maximumDegree(difference).isEmpty || maximumDegree(difference).get < maximumDegree(x).get)
          val (restOfQuotient, remainder) = quotientRemainder(difference, y)

          (add(quotientLeadingTerm, restOfQuotient), remainder)
        })//.ensuring({ qr => println("result: " + qr); x == add(multiply(qr._1, y), qr._2) })
      }
    }
  }

  def squareFreeFactorization(p: P): Stream[P] = {
    maximumDegree(p) match {
      case None | Some(0) => Stream(p)
      case _ => gcd(p, formalDerivative(p)) match {
        case q if maximumDegree(q).get == 0 => Stream(q)
        case q => q #:: squareFreeFactorization(quotient(p, q))
      }
    }
  }

  def removeMultipleRoots(p: P): P = squareFreeFactorization(p).head

  def sturmSequence(p: P): Stream[P] = {
    def stream: Stream[P] = p #:: formalDerivative(p) #:: (stream.zip(stream.tail).map({
      case (a, b) => negate(remainder(a, b))
    }))
    stream.takeWhile(q => maximumDegree(q).nonEmpty)
  }
}

object PolynomialAlgebraOverField {
  trait PolynomialAlgebraOverFieldForMaps[A] extends PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverField[A, Map[Int, A]] {
    override def ring: Field[A]
  }

  implicit def forMaps[A: Field]: PolynomialAlgebraOverField[A, Map[Int, A]] = new PolynomialAlgebraOverFieldForMaps[A] {
    override def ring = implicitly[Field[A]]
  }
  implicit def over[A: Field]: PolynomialAlgebraOverField[A, Polynomial[A]] = PolynomialsOverField.over[A]
}

abstract class PolynomialsOverField[A: Field] extends Polynomials[A] with PolynomialAlgebraOverField[A, Polynomial[A]]
object PolynomialsOverField {
  implicit def over[A: Field]: PolynomialsOverField[A] = new PolynomialsOverField[A] {
    override def ring = implicitly[Field[A]]
  }
}
