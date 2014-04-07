package net.tqft.toolkit.algebra.numberfields

import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.grouptheory.FiniteGroup
import net.tqft.toolkit.algebra.grouptheory.Representation
import net.tqft.toolkit.permutations.Permutations.Permutation
import net.tqft.toolkit.algebra._

abstract class PolynomialQuotientRing[A: EuclideanRing] extends PolynomialsOverEuclideanRing[A] {
  def generator: Polynomial[A]
  override def multiply(a: Polynomial[A], b: Polynomial[A]) = remainder(multiply(a, b), generator)
  def normalForm(p: Polynomial[A]) = remainder(p, generator)
}

object PolynomialQuotientRing {
   def apply[A: EuclideanRing](p: Polynomial[A]) = new PolynomialQuotientRing[A] {
     override def ring = implicitly[EuclideanRing[A]]
     override def generator = p
   }
}

abstract class NumberField[A: Field] extends PolynomialQuotientRing[A] with Field[Polynomial[A]] with VectorSpace[A, Polynomial[A]] {
  override def ring: Field[A] = implicitly[Field[A]]
  override def coefficients: Field[A] = implicitly[Field[A]]
  lazy val rank = maximumDegree(generator).get

  private val powers = {
    import net.tqft.toolkit.functions.Memo._
    def f(n: Int) = normalForm(monomial(n))
    (f _).memo
  }
  override def inverse(q: Polynomial[A]) = {
    if (q == zero) throw new ArithmeticException("/ by zero")
    val (_, b, u) = (extendedEuclideanAlgorithm(generator, q))
    require(maximumDegree(u) == Some(0))
    scalarMultiply(coefficients.inverse(constantTerm(u)), b)
  }

  def minimalPolynomial(p: Polynomial[A]): Polynomial[A] = {
    ???
  }
  val galoisGroup: FiniteGroup[Permutation]
  def galoisGroupAction: galoisGroup.Action[Polynomial[A]]
  def galoisConjugates(p: Polynomial[A]): Set[Polynomial[A]] = galoisGroupAction.orbit(p)
}

object NumberField {
  def apply[A: Field](p: Polynomial[A]): NumberField[A] = new NumberField[A] {
    override val generator = p
    override val coefficients = implicitly[Field[A]]
    override lazy val galoisGroup = ???
    override lazy val galoisGroupAction = ???
  }

  def cyclotomic[A: Field](n: Int): CyclotomicNumberField[A] = new CyclotomicNumberField[A] {
    override val order = n
    override val coefficients = implicitly[Field[A]]
  }
}

