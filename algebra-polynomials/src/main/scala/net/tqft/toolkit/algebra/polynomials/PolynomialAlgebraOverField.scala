package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.EuclideanRing
import net.tqft.toolkit.algebra.GCDRing
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.PrimeField
import net.tqft.toolkit.algebra.numberfields.NumberField

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

          require(ax != ring.zero)
          require(ay != ring.zero)

          val q = ring.quotient(ax, ay)

          val quotientLeadingTerm = monomial(dx - dy, q)
          val difference = add(x, negate(multiply(quotientLeadingTerm, y)))
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

abstract class PolynomialsOverField[A: Field] extends PolynomialsOverGCDRing[A] with PolynomialAlgebraOverField[A, Polynomial[A]]
object PolynomialsOverField {
  implicit def over[A: Field]: PolynomialsOverField[A] = new PolynomialsOverField[A] {
    override def ring = implicitly[Field[A]]
  }
}

trait FiniteField[I] extends Field[Polynomial[I]] {
  def integers: IntegerModel[I]
  def characteristic: I
  def exponent: Int
  lazy val order = integers.power(characteristic, exponent)
}

object FiniteField {
  def apply[I: IntegerModel](p: I, n: Int): FiniteField[I] = new NumberField[I]()(PrimeField(p)) with FiniteField[I] {
    override val generator = findIrreduciblePolynomial(n, p)
    override val coefficients = PrimeField(p)
    override val exponent = n
    override val characteristic = p
    override val integers = implicitly[IntegerModel[I]]
  }

  def findIrreduciblePolynomial[I: IntegerModel](degree: Int, prime: I): Polynomial[I] = {
    def integers = implicitly[IntegerModel[I]]
    if (degree == 1) {
      implicitly[Polynomials[I]].monomial(1, integers.one)
    } else {
      def randomPolynomial: Polynomial[I] = IndexedSeq.fill(degree + 1)(integers.fromInt(scala.util.Random.nextInt))
      val polynomials = PolynomialsOverFiniteField.over(FiniteField(prime, 1))
      Iterator.continually(randomPolynomial).find(p => polynomials.irreducible_?(p)).get
    }
  }
}

abstract class PolynomialsOverFiniteField[I: FiniteField] extends PolynomialsOverField[Polynomial[I]] {
	def irreducible_?(p: Polynomial[I]): Boolean = ???
}

object PolynomialsOverFiniteField {
  def over[I: FiniteField]: PolynomialsOverFiniteField[I] = new PolynomialsOverFiniteField[I] {
    override def ring = implicitly[FiniteField[I]]
  }
}
