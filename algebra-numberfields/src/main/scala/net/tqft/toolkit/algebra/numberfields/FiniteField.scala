package net.tqft.toolkit.algebra.numberfields

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._

trait FiniteField[I] extends Field[Polynomial[I]] with Finite[Polynomial[I]] {
  def integers: IntegerModel[I]
  def characteristic: I
  def exponent: Int
  lazy val order = integers.power(characteristic, exponent)
  
  override def elements = {
    ???
  }
  def randomElement: Polynomial[I] = {
    implicit def zero = integers
    IndexedSeq.fill(exponent)(integers.fromInt(scala.util.Random.nextInt(Integers.from(characteristic))))
  }

  override def toString = {
    s"FiniteField($characteristic, $exponent)"
  }
}

object FiniteField {
  def apply[I: IntegerModel](p: I, n: Int, polynomial: Polynomial[I]) = {
    new NumberField[I]()(PrimeField(p)) with FiniteField[I] {
      override val generator = polynomial
      override val ring = PrimeField(p)
      override val exponent = n
      override val characteristic = p
      override val integers = implicitly[IntegerModel[I]]

      override def toString = {
        s"FiniteField(p = $characteristic, n = $exponent, generator = $generator)"
      }
    }
  }

  def apply[I: IntegerModel](p: I, n: Int): FiniteField[I] = apply(p, n, findIrreduciblePolynomial(n, p))

  private def findIrreduciblePolynomial[I: IntegerModel](degree: Int, prime: I): Polynomial[I] = {
    def integers = implicitly[IntegerModel[I]]
    if (degree == 1) {
      implicitly[Polynomials[I]].monomial(1, integers.one)
    } else {
      val k = integers.toBigInt(prime).%(Integer.MAX_VALUE).toInt
      def randomPolynomial: Polynomial[I] = IndexedSeq.fill(degree)(integers.fromInt(scala.util.Random.nextInt(k))) :+ integers.one
      val polynomials = PolynomialsOverFiniteField.over(FiniteField(prime, 1))
      Iterator.continually(randomPolynomial).find(p => polynomials.irreducible_?(p)).get
    }
  }
}
