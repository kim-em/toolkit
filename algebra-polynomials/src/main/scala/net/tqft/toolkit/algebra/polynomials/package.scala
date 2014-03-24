package net.tqft.toolkit.algebra

package object polynomials {
  type =>?[-A, +B] = PartialFunction[A, B]

  type RationalFunction[A] = Fraction[Polynomial[Fraction[A]]]
  type MultivariableRationalFunction[A, V] = Fraction[MultivariablePolynomial[Fraction[A], V]]
}