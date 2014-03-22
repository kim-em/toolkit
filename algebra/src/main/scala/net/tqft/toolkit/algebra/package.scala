package net.tqft.toolkit

import net.tqft.toolkit.algebra.polynomials.Polynomial

package object algebra {
	type RationalFunction[X] = Fraction[Polynomial[Fraction[X]]]
}