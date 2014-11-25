package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

trait PolynomialAlgebraOverField[A, P] extends PolynomialAlgebraOverGCDRing[A, P] with EuclideanRing[P] {
  override def ring: Field[A]

  override def quotientRemainder(x: P, y: P): (P, P) = {
    //    println(s"beginning quotientRemainder($x, $y)")
    (maximumDegree(x), maximumDegree(y)) match {
      case (_, None) => throw new ArithmeticException
      case (None, Some(dy)) => (zero, zero)
      case (Some(dx), Some(dy)) => {
        (if (dy > dx) {
          (zero, x)
        } else {
          val ax = leadingCoefficient(x).get
          val ay = leadingCoefficient(y).get

          require(!ring.zero_?(ax))
          require(!ring.zero_?(ay))

          val q = ring.quotient(ax, ay)

          val quotientLeadingTerm = monomial(dx - dy, q)
          val difference = add(x, negate(multiply(quotientLeadingTerm, y)))
          val f0 = multiply(quotientLeadingTerm, y)
          val f1 = negate(multiply(quotientLeadingTerm, y))
          require(maximumDegree(difference).isEmpty || maximumDegree(difference).get < maximumDegree(x).get)
          val (restOfQuotient, remainder) = quotientRemainder(difference, y)

          (add(quotientLeadingTerm, restOfQuotient), remainder)
        }) //.ensuring({ qr => println("result: " + qr); x == add(multiply(qr._1, y), qr._2) })
      }
    }
  }

  def sturmSequence(p: P): Stream[P] = {
    def stream: Stream[P] = p #:: formalDerivative(p) #:: (stream.zip(stream.tail).map({
      case (a, b) => negate(remainder(a, b))
    }))
    stream.takeWhile(q => maximumDegree(q).nonEmpty)
  }
}

object PolynomialAlgebraOverField {
  trait PolynomialAlgebraOverFieldForMaps[A] extends PolynomialAlgebraOverGCDRing.PolynomialAlgebraOverGCDRingForMaps[A] with PolynomialAlgebraOverField[A, Map[Int, A]] {
    override def ring: Field[A]
  }

  implicit def forMaps[A: Field]: PolynomialAlgebraOverField[A, Map[Int, A]] = new PolynomialAlgebraOverFieldForMaps[A] {
    override def ring = implicitly[Field[A]]
  }
  implicit def over[A: Field]: PolynomialAlgebraOverField[A, Polynomial[A]] = PolynomialsOverField.over[A]
}

abstract class PolynomialsOverField[A: Field] extends PolynomialsOverGCDRing[A] with PolynomialAlgebraOverField[A, Polynomial[A]] {
    override def toString = s"PolynomialsOverField.over($ring)"

}
object PolynomialsOverField {
  implicit def over[A: Field]: PolynomialsOverField[A] = new PolynomialsOverField[A] {
    override def ring = implicitly[Field[A]]
  }
}