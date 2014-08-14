package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.spiders._

abstract class CubicSpider[R: Field] extends TrivalentSpider[R] { cs =>
  lazy val squareReduction: Reduction[PlanarGraph, R] = {
    object PreCubicSpider extends TrivalentSpider[R] {
      override def ring = cs.ring
      override def omega = cs.omega
      override def d = cs.d
      override def b = cs.b
      override def t = cs.t
    }
    PreCubicSpider.basis(4, PreCubicSpider.reducedDiagrams(4, 0) ++ PreCubicSpider.reducedDiagrams(4, 2)).deriveNewRelations(4).next
  }
  override def reductions = super.reductions :+ squareReduction
}

object CubicSpider extends CubicSpider[MultivariableRationalFunction[Fraction[BigInt], String]] with IntegerMultivariableRationalFunctionTrivalentSpider {
  override val omega = ring.one

  //  def z: MultivariableRationalFunction[BigInt, String] = Map(Map("z" -> 1) -> 1) // inverse of MultivariablePolynomial(Map(Map("b" -> 1, "d" -> 1) -> 1, Map("d" -> 1, "t" -> 1) -> 1, Map("t" -> 1) -> 1)) // b d + t + d t

  //  override def alpha = ring.multiply(z, Map(Map("b" -> 3) -> 1, Map("b" -> 2, "t" -> 1) -> 1, Map("b" -> 1, "t" -> 2) -> -1)) // b^3 + b^2 t - b t^2
  //  override def beta = ring.multiply(z, Map(Map("b" -> 2) -> -1, Map("t" -> 2) -> 1, Map("d" -> 1, "t" -> 2) -> 1)) // -b^2 + t^2 + d t^2
}

object TwistedCubicSpider extends CubicSpider[MultivariableRationalFunction[Polynomial[Fraction[BigInt]], String]] with TwistedMultivariableRationalFunctionTrivalentSpider {
  //  def `d^(-1)`: MultivariableRationalFunction[Polynomial[Fraction[Int]], String] = Map(Map("(d^(-1))" -> 1) -> 1)
  //  override def alpha = ring.multiply(`d^(-1)`, Map(Map("b" -> 2) -> 1))
  //  override def beta = ring.multiply(`d^(-1)`, Map(Map("b" -> 1) -> -1))
}