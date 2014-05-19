package net.tqft.toolkit.algebra.numberfields

import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra._

abstract class PolynomialQuotientRing[A: Field] extends PolynomialsOverField[A] {
  protected val polynomials = implicitly[PolynomialsOverField[A]]

  def generator: Polynomial[A]
  lazy val degree = maximumDegree(generator).get

  
  override def multiply(a: Polynomial[A], b: Polynomial[A]) = polynomials.remainder(polynomials.multiply(a, b), generator)
  def normalForm(p: Polynomial[A]) = polynomials.remainder(p, generator)
  
  override def toString = s"PolynomialQuotientRing($generator)(${implicitly[Field[A]]})"
}

object PolynomialQuotientRing {
  def apply[A: Field](p: Polynomial[A]) = new PolynomialQuotientRing[A] {
    override def ring = implicitly[Field[A]]
    override def generator = p
  }
}

abstract class NumberField[A: Field] extends PolynomialQuotientRing[A] with Field[Polynomial[A]] {
  override def ring: Field[A] = implicitly[Field[A]]

  private val powers = {
    import net.tqft.toolkit.functions.Memo._
    def f(n: Int) = normalForm(monomial(n))
    (f _).memo
  }
  override def inverse(q: Polynomial[A]) = {
    if (zero_?(q)) throw new ArithmeticException("/ by zero")
    val (_, b, u) = (polynomials.extendedEuclideanAlgorithm(generator, q))
    require(maximumDegree(u) == Some(0))
    require(maximumDegree(b).get < maximumDegree(generator).get)
    scalarMultiply(ring.inverse(constantTerm(u)), b)
  }

}

object NumberField {
  def apply[A: GCDRing](p: Polynomial[A]): NumberField[Fraction[A]] = new NumberField[Fraction[A]] {
    override val generator = p.mapValues(x => x: Fraction[A])
  }

  def cyclotomic[A: GCDRing](n: Int): CyclotomicNumberField[Fraction[A]] = new CyclotomicNumberField[Fraction[A]] {
    override val order = n
  }
}

