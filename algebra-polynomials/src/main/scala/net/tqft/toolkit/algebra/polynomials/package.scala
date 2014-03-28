package net.tqft.toolkit.algebra

package object polynomials {
  type =>?[-A, +B] = PartialFunction[A, B]

  type RationalFunction[A] = Fraction[Polynomial[A]]
  type MultivariableRationalFunction[A, V] = Fraction[MultivariablePolynomial[A, V]]
}