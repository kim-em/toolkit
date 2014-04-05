package net.tqft.toolkit.algebra.numberfields

import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.VectorSpace
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.grouptheory.FiniteGroup
import net.tqft.toolkit.algebra.grouptheory.Representation
import net.tqft.toolkit.permutations.Permutations.Permutation

trait NumberField[A] extends Field[Polynomial[A]] with VectorSpace[A, Polynomial[A]] {
  override implicit def coefficients: Field[A]
  val generator: Polynomial[A]
  lazy val rank = generator.maximumDegree.get

  protected lazy val polynomials = implicitly[PolynomialsOverField[A]] // has to be lazy so coefficientField is available

  private val powers = {
    import net.tqft.toolkit.functions.Memo._
    def f(n: Int) = polynomials.remainder(polynomials.monomial(n), generator)
    (f _).memo
  }
  def normalize(q: Polynomial[A]) = polynomials.remainder(q, generator)

  override def fromInt(x: Int) = polynomials.fromInt(x)
  override def inverse(q: Polynomial[A]) = {
    if (q == zero) throw new ArithmeticException("/ by zero")
    val (_, b, u) = (polynomials.extendedEuclideanAlgorithm(generator, q))
    require(u.maximumDegree == Some(0))
    scalarMultiply(coefficients.inverse(u.constantTerm), b)
  }
  override def negate(q: Polynomial[A]) = polynomials.negate(q)
  override lazy val zero = polynomials.zero
  override lazy val one = polynomials.one
  override def multiply(a: Polynomial[A], b: Polynomial[A]) = normalize(polynomials.multiply(a, b))
  override def add(a: Polynomial[A], b: Polynomial[A]) = polynomials.add(a, b)

  def scalarMultiply(a: A, p: Polynomial[A]): Polynomial[A] = polynomials.scalarMultiply(a, p)
  
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

