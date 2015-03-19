package net.tqft.toolkit

//import net.tqft.toolkit.algebra.polynomials.Polynomial
//import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial

package object algebra {
  
    def Rationals = implicitly[OrderedField[Fraction[Int]]]
    def Doubles = implicitly[OrderedField[Double]]
}