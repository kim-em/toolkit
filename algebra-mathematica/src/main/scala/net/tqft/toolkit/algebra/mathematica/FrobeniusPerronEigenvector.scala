package net.tqft.toolkit.algebra.mathematica

import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.mathematica._

case class FrobeniusPerronEigenvectorData(
  generator: Polynomial[Fraction[Int]],
  generatorApproximation: Double,
  eigenvalue: Polynomial[Fraction[Int]],
  eigenvalueApproximation: Double,
  eigenvector: Seq[Polynomial[Fraction[Int]]],
  eigenvectorApproximation: Seq[Double])

object FrobeniusPerronEigenvector {
  def apply(m: Seq[Seq[Int]]): FrobeniusPerronEigenvectorData = {
    val input = FullFormExpression(SymbolExpression("ToNumberField"),
      Seq(
        FullFormExpression(SymbolExpression("Flatten"), Seq(
          FullFormExpression(SymbolExpression("Eigensystem"),
            Seq(
              m, 1))))))
    ???
  }
}