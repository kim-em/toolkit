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
