package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.EuclideanRing
import net.tqft.toolkit.algebra.GCDRing
import net.tqft.toolkit.algebra.Fraction

trait PolynomialAlgebraOverGCDRing[A, P] extends PolynomialAlgebra[A, P] with GCDRing[P] {
  override implicit def ring: GCDRing[A]

  def content(p: P): A = {
    ring.gcd(toMap(p).values.toSeq: _*)
  }
  def primitivePart(p: P): P = {
    val c = content(p)
    fromMap(toMap(p).mapValues(x => ring.exactQuotient(x, c)))
  }

  def subresultant_gcd(a: P, b: P): P = {
    val gcdRing = implicitly[GCDRing[A]]

    if (toMap(a).isEmpty && toMap(b).isEmpty) {
      one
    } else if (toMap(a).isEmpty) {
      b
    } else if (toMap(b).isEmpty) {
      a
    } else {

      def _remainder(x: P, y: P): P = {
        // TODO can we do this better?
        // not unless we have a EuclideanRing[A]?
        val rationalPolynomials = implicitly[PolynomialsOverEuclideanRing[Fraction[A]]]
        fromMap(rationalPolynomials.remainder(toMap(x).mapValues(a => (a: Fraction[A])), toMap(y).mapValues(a => (a: Fraction[A]))).coefficients.mapValues(f => f.ensuring(_.denominator == gcdRing.one).numerator))
      }

      var r0 = a
      var r1 = b
      var d = maximumDegree(a).get - maximumDegree(b).get
      if (d < 0) {
        d = -d
        r0 = b
        r1 = a
      }
      var gamma = leadingCoefficient(r1).get
      var oldgamma = gamma
      var beta = gcdRing.fromInt(if (d % 2 == 0) -1 else 1)
      var psi = gcdRing.fromInt(-1)
      var done = r1 ==zero
      while (!done) {
        val oldr1 = r1
        r1 = fromMap(toMap(_remainder(scalarMultiply(gcdRing.power(gamma, d + 1), r0), r1)).mapValues(c => gcdRing.exactQuotient(c, beta)))
        r0 = oldr1
        done = r1 == zero
        if (!done) {
          oldgamma = gamma
          gamma = leadingCoefficient(r1).get
          psi = gcdRing.exactQuotient(gcdRing.power(gcdRing.negate(gamma), d), gcdRing.power(psi, d - 1))
          d = maximumDegree(r0).get - maximumDegree(r1).get
          beta = gcdRing.negate(gcdRing.multiply(oldgamma, gcdRing.power(psi, d)))
        }
      }
      r0
    }
  }

  override def gcd(x: P, y: P): P = {
    val xc = content(x)
    val yc = content(y)
    val contentGCD = ring.gcd(xc, yc)

    val primitive = primitivePart(subresultant_gcd(x, y))

    val result = scalarMultiply(
      contentGCD,
      primitive)

    // TODO remove:
    //    require(exactQuotientOption(x, result).nonEmpty)
    //    require(exactQuotientOption(y, result).nonEmpty)

    result
  }
  override def exactQuotientOption(x: P, y: P): Option[P] = {
    (maximumDegree(x), maximumDegree(y)) match {
      case (_, None) => throw new ArithmeticException
      case (None, Some(dy)) => Some(zero)
      case (Some(dx), Some(dy)) => {
        if (dy > dx) {
          None
        } else {
          val ax = leadingCoefficient(x).get
          val ay = leadingCoefficient(y).get

          require(ax != ring.zero)
          require(ay != ring.zero)

          ring.exactQuotientOption(ax, ay) match {
            case None => None
            case Some(q) => {
              val quotientLeadingTerm = monomial(dx - dy, q)
              val difference = add(x, negate(multiply(quotientLeadingTerm, y)))
              require(maximumDegree(difference).isEmpty || maximumDegree(difference).get < maximumDegree(x).get)
              exactQuotientOption(difference, y).map({ restOfQuotient => add(quotientLeadingTerm, restOfQuotient) })
            }
          }
        }
      }
    }
  }
}

object PolynomialAlgebraOverGCDRing {
  trait PolynomialAlgebraOverGCDRingForMaps[A] extends PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverGCDRing[A, Map[Int, A]] {
    override def ring: GCDRing[A]
  }

  implicit def forMaps[A: GCDRing]: PolynomialAlgebraOverGCDRing[A, Map[Int, A]] = new PolynomialAlgebraOverGCDRingForMaps[A] {
    override def ring = implicitly[GCDRing[A]]
  }
  implicit def over[A: GCDRing]: PolynomialAlgebraOverGCDRing[A, Polynomial[A]] = PolynomialsOverGCDRing.over[A]
}

abstract class PolynomialsOverGCDRing[A: GCDRing] extends Polynomials[A] with PolynomialAlgebraOverGCDRing[A, Polynomial[A]]
object PolynomialsOverGCDRing {
  implicit def over[A: GCDRing]: PolynomialsOverGCDRing[A] = new PolynomialsOverGCDRing[A] {
    override def ring = implicitly[GCDRing[A]]
  }
}

trait PolynomialAlgebraOverEuclideanRing[A, P] extends PolynomialAlgebraOverGCDRing[A, P] with EuclideanRing[P] {
  override def ring: EuclideanRing[A]

  override def quotientRemainder(x: P, y: P): (P, P) = {
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
          require(maximumDegree(difference).isEmpty || maximumDegree(difference).get < maximumDegree(x).get)
          val (restOfQuotient, remainder) = quotientRemainder(difference, y)

          (add(quotientLeadingTerm, restOfQuotient), remainder)
        }
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

object PolynomialAlgebraOverEuclideanRing {
  trait PolynomialAlgebraOverEuclideanRingForMaps[A] extends PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverEuclideanRing[A, Map[Int, A]] {
    override def ring: EuclideanRing[A]
  }

  implicit def forMaps[A: EuclideanRing]: PolynomialAlgebraOverEuclideanRing[A, Map[Int, A]] = new PolynomialAlgebraOverEuclideanRingForMaps[A] {
    override def ring = implicitly[EuclideanRing[A]]
  }
  implicit def over[A: EuclideanRing]: PolynomialAlgebraOverEuclideanRing[A, Polynomial[A]] = PolynomialsOverEuclideanRing.over[A]
}

abstract class PolynomialsOverEuclideanRing[A: EuclideanRing] extends Polynomials[A] with PolynomialAlgebraOverEuclideanRing[A, Polynomial[A]]
object PolynomialsOverEuclideanRing {
  implicit def over[A: EuclideanRing]: PolynomialsOverEuclideanRing[A] = new PolynomialsOverEuclideanRing[A] {
    override def ring = implicitly[EuclideanRing[A]]
  }
}
