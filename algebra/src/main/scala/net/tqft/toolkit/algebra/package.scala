package net.tqft.toolkit

import net.tqft.toolkit.algebra.polynomials.Polynomial

package object algebra {
  type RationalFunction[A] = Fraction[Polynomial[Fraction[A]]]

}