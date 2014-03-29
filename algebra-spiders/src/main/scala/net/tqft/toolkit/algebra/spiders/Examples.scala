package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.polynomials.RationalFunction
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction
import net.tqft.toolkit.algebra.Fraction


abstract class TrivalentSpider[R: Ring] extends PlanarGraphReductionSpider[R] {
  def d: R
  def b: R
  def t: R
  def omega: R
  override def eigenvalue(label: Int) = {
    label match {
      case -1 => ???
      case 2 => ring.one
      case 3 => omega
    }
  }

  private val loopReduction = Reduction(PlanarGraph.loop, Map(PlanarGraph.empty -> d))
  private val bigonReduction = Reduction(PlanarGraph.polygon(2), Map(PlanarGraph.strand -> b))
  private val triangleReduction = Reduction(PlanarGraph.polygon(3), Map(PlanarGraph.star(3) -> t))
  override def reductions = Seq(loopReduction, bigonReduction, triangleReduction)
}

abstract class CubicSpider[R: Ring] extends TrivalentSpider[R] {
  override val omega = ring.one

  private val squareReduction = ???
  override def reductions = super.reductions :+ squareReduction
}

object `SO(3)_q` extends CubicSpider[RationalFunction[Fraction[Int]]] {
  override val ring = implicitly[Field[RationalFunction[Fraction[Int]]]]

  val q: RationalFunction[Fraction[Int]] = Polynomial(1 -> Fraction(1, 1))
  override val d = ring.add(q, ring.inverse(q))
  override val b = ???
  override val t = ???

  private val pentagonReduction = ???
  override def reductions = super.reductions :+ pentagonReduction
}

object `(G_2)_q` extends CubicSpider[RationalFunction[Fraction[Int]]] {
  override val ring = implicitly[Field[RationalFunction[Fraction[Int]]]]

  val q: RationalFunction[Fraction[Int]] = Polynomial(1 -> Fraction(1, 1))
  override val d = ???
  override val b = ???
  override val t = ???

  private val pentagonReduction = ???
  override def reductions = super.reductions :+ pentagonReduction
}

sealed trait QuantumExceptionalVariable
case object v extends QuantumExceptionalVariable
case object w extends QuantumExceptionalVariable
object QuantumExceptionalVariable {
  implicit val ordering: Ordering[QuantumExceptionalVariable] = Ordering.by({
    case `v` => 1
    case `w` => 2
  })
}

import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial._

object QuantumExceptional extends TrivalentSpider[MultivariableRationalFunction[Fraction[Int], QuantumExceptionalVariable]] {
  override val ring = implicitly[Field[MultivariableRationalFunction[Fraction[Int], QuantumExceptionalVariable]]]

  override val omega = ring.one
  override val d = ???
  override val b = ???
  override val t = ???
  override def reductions = super.reductions
}
