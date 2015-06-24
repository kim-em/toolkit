package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.spiders._

object QuantumExceptionalSeries extends BraidedTrivalentSpider[MultivariableRationalFunction[Fraction[BigInt], String]] with RationalFunctionPolyhedronNamer[Fraction[BigInt]] {
  override def coefficientRing = implicitly[Field[Fraction[BigInt]]]
  override def ring = implicitly[Field[MultivariableRationalFunction[Fraction[BigInt], String]]]

  override val omega = ring.one

  val v: MultivariableRationalFunction[Fraction[BigInt], String] = Map(Map("v" -> 1) -> 1)
  val w: MultivariableRationalFunction[Fraction[BigInt], String] = Map(Map("w" -> 1) -> 1)

  import AlgebraicNotation._

  def bracket(k: Int, l: Int) = (w ^ k) * (v ^ k) - (w ^ (-l)) * (v ^ (-k))
  def brace(k: Int, l: Int) = (w ^ k) * (v ^ k) + (w ^ (-l)) * (v ^ (-k))

  override val d = -((brace(0, 2) * bracket(1, 5) * bracket(1, -6)) / (bracket(1, 0) * bracket(1, -1)))
  override val b = (brace(1, 2) * brace(1, -3) * bracket(0, 3)) / bracket(0, 1)
  override val t = brace(0, 1) * ((v + (v ^ (-1))) * ((w ^ 2) * (v ^ (-1)) + (w ^ (-2)) * v) + (v ^ 4) - (v ^ 2) - 1 - (v ^ (-2)) + (v ^ (-4)))
  override def z = -(v ^ 6)

  override def reductions = super.reductions
}