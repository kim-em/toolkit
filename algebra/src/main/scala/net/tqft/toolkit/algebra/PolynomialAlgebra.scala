package net.tqft.toolkit.algebra


trait PolynomialAlgebra[A] extends FreeModuleOnMonoid[A, Int, Polynomial[A]] with AssociativeAlgebra[A, Polynomial[A]] {

  def monomial(k: Int): Polynomial[A] = monomial(k, ring.one)
  def monomial(k: Int, a: A): Polynomial[A] = Polynomial((k, a))

  def constant(a: A) = monomial(0, a)
  override def fromInt(x: Int): Polynomial[A] = constant(ring.fromInt(x))

  override val monoid = Gadgets.Integers

  override def wrap(terms: List[(Int, A)]): Polynomial[A] = new PolynomialImpl(terms)
  private class PolynomialImpl(_terms: List[(Int, A)]) extends Polynomial[A] {
    val terms = reduce(_terms)
  }

  def composeAsFunctions(p: Polynomial[A], q: Polynomial[A]): Polynomial[A] = {
    add(p.terms map { case (e, a) => scalarMultiply(a, power(q, e)) })
  }

  def formalDerivative(p: Polynomial[A]): Polynomial[A] = {
    Polynomial((p.terms map { case (0, _) => (0, ring.zero); case (k, a) => (k - 1, ring.multiplyByInt(a, k)) }): _*)
  }
}
