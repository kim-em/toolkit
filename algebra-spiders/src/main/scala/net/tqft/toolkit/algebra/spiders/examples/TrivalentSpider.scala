package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.numberfields.NumberField

abstract class TrivalentSpider[R: Field] extends PlanarGraphReductionSpiderOverField[R] {
  override lazy val vertexTypes = Seq(VertexType(3, 1))

  def d: R
  def b: R
  def t: R
  def omega: R
  override def eigenvalue(label: Int) = {
    label match {
      case 3 => omega
    }
  }

  private lazy val loopReduction = Reduction(PlanarGraph.loop, Map(PlanarGraph.empty -> d))
  private lazy val monogonReduction = Reduction(PlanarGraph.polygon(1), Map(PlanarGraph.polygon(1) -> coefficients.zero))
  private lazy val bigonReduction = Reduction(PlanarGraph.polygon(2), Map(PlanarGraph.strand -> b))
  private lazy val triangleReduction = Reduction(PlanarGraph.polygon(3), Map(PlanarGraph.star(3) -> t))
  override def reductions = Seq(loopReduction, monogonReduction, bigonReduction, triangleReduction)
}


trait MultivariableRationalFunctionTrivalentSpider[A] extends TrivalentSpider[MultivariableRationalFunction[A, String]] with MultivariableRationalFunctionSpider[A]  {
  override def d = Map(Map("d" -> 1) -> coefficientRing.one)
  override def b = Map(Map("b" -> 1) -> coefficientRing.one)
  override def t = Map(Map("t" -> 1) -> coefficientRing.one)
}

trait IntegerMultivariableRationalFunctionTrivalentSpider extends MultivariableRationalFunctionTrivalentSpider[BigInt] {
  override def omega = 1
  override def coefficientRing = implicitly[EuclideanRing[BigInt]]
}
trait TwistedMultivariableRationalFunctionTrivalentSpider extends MultivariableRationalFunctionTrivalentSpider[Polynomial[Fraction[Int]]] {
  override def t = ring.zero
  // TODO improve implicits here?:
  override def omega = MultivariablePolynomial.constant[Polynomial[Fraction[Int]], String](Polynomial[Fraction[Int]](Map(1 -> Fraction.whole(1))))
  override def coefficientRing: Field[Polynomial[Fraction[Int]]] = NumberField.cyclotomic[Int](3)
}

object TrivalentSpider extends IntegerMultivariableRationalFunctionTrivalentSpider

object TwistedTrivalentSpider extends TwistedMultivariableRationalFunctionTrivalentSpider
