package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.polynomials.RationalFunction
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial._
import net.tqft.toolkit.algebra.numberfields.NumberField
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.EuclideanRing
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebraOverEuclideanRing

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
  private val monogonReduction = Reduction(PlanarGraph.polygon(1), Map(PlanarGraph.polygon(1) -> coefficients.zero))
  private val bigonReduction = Reduction(PlanarGraph.polygon(2), Map(PlanarGraph.strand -> b))
  private val triangleReduction = Reduction(PlanarGraph.polygon(3), Map(PlanarGraph.star(3) -> t))
  override def reductions = Seq(loopReduction, monogonReduction, bigonReduction, triangleReduction)
}

trait PolynomialTrivalentSpider[A] extends TrivalentSpider[MultivariablePolynomial[A, String]] {
  def coefficientRing: Ring[A]
  override def d = Map(Map("d" -> 1) -> coefficientRing.one)
  override def b = Map(Map("b" -> 1) -> coefficientRing.one)
  override def t = Map(Map("t" -> 1) -> coefficientRing.one)
}

trait RationalPolynomialTrivalentSpider extends PolynomialTrivalentSpider[Fraction[Int]] {
  override def coefficientRing = implicitly[Ring[Fraction[Int]]]
}

object Trivalent { trivalent =>
  type R = MultivariablePolynomial[Fraction[Int], String]

  implicit val ring = {
    implicitly[Ring[R]]
  }

  object TrivalentSpider extends TrivalentSpider[R] with RationalPolynomialTrivalentSpider {
    override def omega = 1
    override def ring = trivalent.ring
  }
}

object TwistedTrivalent {
  implicit val numberField: Field[Polynomial[Fraction[Int]]] = NumberField.cyclotomic[Fraction[Int]](3)
  implicit val polynomialRing = MultivariablePolynomialAlgebraOverEuclideanRing.over[Polynomial[Fraction[Int]], String]
  type R = MultivariablePolynomial[Polynomial[Fraction[Int]], String]
  implicit val ring = {
    implicitly[Ring[R]]
  }

  object TwistedTrivalentSpider extends TrivalentSpider[R] {
    override def t = ring.zero
    override def d = MultivariablePolynomial(Map(Map("d" -> 1) -> 1))
    override def b = MultivariablePolynomial(Map(Map("b" -> 1) -> 1))
    override def omega = MultivariablePolynomial.constant[Polynomial[Fraction[Int]], String](Polynomial[Fraction[Int]](Map(1 -> Fraction.whole(1))))

    override def ring = TwistedTrivalent.ring
  }
}

abstract class CubicSpider[R: Ring] extends TrivalentSpider[R] {
  def alpha: R
  def beta: R

  private val squareReduction = Reduction(PlanarGraph.polygon(4),
    Map(
      PlanarGraph.two_strands_horizontal -> alpha,
      PlanarGraph.two_strands_vertical -> alpha,
      PlanarGraph.I -> beta,
      PlanarGraph.H -> beta))
  override def reductions = super.reductions :+ squareReduction
}

object Cubic {
  type R = MultivariablePolynomial[Fraction[Int], String]

  implicit val ring = {
    implicitly[Ring[R]]
  }

  object CubicSpider extends CubicSpider[R] with RationalPolynomialTrivalentSpider {
    override val omega = ring.one

    val z: MultivariablePolynomial[Fraction[Int], String] = Map(Map("z" -> 1) -> 1) // MultivariablePolynomial(Map(Map("b" -> 1, "d" -> 1) -> 1, Map("d" -> 1, "t" -> 1) -> 1, Map("t" -> 1) -> 1)) // b d + t + d t

    override def alpha = ring.multiply(z, Map(Map("b" -> 3) -> 1, Map("b" -> 2, "t" -> 1) -> 1, Map("b" -> 1, "t" -> 2) -> -1)) // b^3 + b^2 t - b t^2
    override def beta = ring.multiply(z, Map(Map("b" -> 2) -> -1, Map("t" -> 2) -> 1, Map("d" -> 1, "t" -> 2) -> 1)) // -b^2 + t^2 + d t^2

    override def ring = Cubic.ring

  }
}

object `SO(3)_q` extends CubicSpider[RationalFunction[Fraction[Int]]] {
  override val ring = implicitly[Field[RationalFunction[Fraction[Int]]]]

  val q: RationalFunction[Fraction[Int]] = Polynomial(1 -> Fraction(1, 1))
  override val d = ring.add(q, ring.inverse(q))
  override val b = ???
  override val t = ???
  override val alpha = ???
  override val beta = ???
  override val omega = ring.one

  private val pentagonReduction = ???
  override def reductions = super.reductions :+ pentagonReduction
}

object `(G_2)_q` extends CubicSpider[RationalFunction[Fraction[Int]]] {
  override val ring = implicitly[Field[RationalFunction[Fraction[Int]]]]

  val q: RationalFunction[Fraction[Int]] = Polynomial(1 -> Fraction(1, 1))
  override val d = ???
  override val b = ???
  override val t = ???
  override val alpha = ???
  override val beta = ???
  override val omega = ring.one

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

object QuantumExceptional extends TrivalentSpider[MultivariablePolynomial[Fraction[Int], QuantumExceptionalVariable]] {
  override val ring = implicitly[Ring[MultivariablePolynomial[Fraction[Int], QuantumExceptionalVariable]]]

  override val omega = ring.one
  override val d = ???
  override val b = ???
  override val t = ???
  override def reductions = super.reductions
}
