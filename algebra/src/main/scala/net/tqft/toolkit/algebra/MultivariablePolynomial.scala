package net.tqft.toolkit.algebra

trait MultivariablePolynomial[A, V] extends LinearCombo[A, Map[V, Int]]

object MultivariablePolynomial {
  def apply[A: Ring, V](terms: (Map[V, Int], A)*) = MultivariablePolynomialAlgebras.over(implicitly[Ring[A]]).wrap(terms.toList)
}
