package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.polynomials.RationalFunction
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial._
import net.tqft.toolkit.algebra.numberfields.NumberField
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebraOverField
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.EuclideanRing

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
  private val monogonReduction = Reduction(PlanarGraph.polygon(1), Map(PlanarGraph.polygon(1) -> 0))
  private val bigonReduction = Reduction(PlanarGraph.polygon(2), Map(PlanarGraph.strand -> b))
  private val triangleReduction = Reduction(PlanarGraph.polygon(3), Map(PlanarGraph.star(3) -> t))
  override def reductions = Seq(loopReduction, bigonReduction, triangleReduction)
}

object Trivalent { trivalent =>
  implicit val ring = {
    implicitly[Ring[MultivariableRationalFunction[Fraction[Int], String]]]
  }
  
  object TrivalentSpider extends TrivalentSpider[MultivariableRationalFunction[Fraction[Int], String]] {
    override def d = Map(Map("d" -> 1) -> 1)
    override def b = Map(Map("b" -> 1) -> 1)
    override def t = Map(Map("t" -> 1) -> 1)
    override def omega = 1
    
    override def ring = trivalent.ring
  }
}

object TwistedCoefficients {
  implicit val numberField: Field[Polynomial[Fraction[Int]]] = NumberField.cyclotomic[Fraction[Int]](3)
  implicit val polynomialRing = {
    def polynomials = MultivariablePolynomialAlgebraOverField.over[Polynomial[Fraction[Int]], String]
  }
  implicit val ring = {
    implicitly[Ring[MultivariableRationalFunction[Polynomial[Fraction[Int]], String]]]
  }

  abstract class TwistedTrivalentSpider extends TrivalentSpider[MultivariableRationalFunction[Polynomial[Fraction[Int]], String]] {
    override def t = ring.zero
    override def omega = MultivariablePolynomial.constant[Polynomial[Fraction[Int]], String](Polynomial[Fraction[Int]](Map(1 -> Fraction.whole(1))))
  }
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

object QuantumExceptional extends TrivalentSpider[MultivariableRationalFunction[Fraction[Int], QuantumExceptionalVariable]] {
  override val ring = implicitly[Field[MultivariableRationalFunction[Fraction[Int], QuantumExceptionalVariable]]]

  override val omega = ring.one
  override val d = ???
  override val b = ???
  override val t = ???
  override def reductions = super.reductions
}
