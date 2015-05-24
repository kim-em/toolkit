package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.numberfields.NumberField

abstract class TetravalentSpider[R: Field] extends PlanarGraphReductionSpiderOverField[R] {
  override lazy val vertexTypes = Seq(VertexType(4, 0, 1))

  def d: R
  def b: R
  override def eigenvalue(label: Int) = {
    label match {
      case 4 => ring.one
    }
  }

  protected val vertex = PlanarGraph.star(4, 1)

  private lazy val loopReduction = Reduction(PlanarGraph.loop, Map(PlanarGraph.empty -> d))
  private lazy val uncappableReduction = Reduction(
    diagramSpider.stitch(vertex),
    Map(PlanarGraph.strand -> ring.zero))
  private lazy val threeStrings = Reduction(
    diagramSpider.multiply(vertex, vertex, 3),
    Map(PlanarGraph.strand -> b))

  override def reductions = Seq(loopReduction, uncappableReduction, threeStrings)
}

trait MultivariableRationalFunctionTetravalentSpider[A] extends TetravalentSpider[MultivariableRationalFunction[A, String]] with MultivariableRationalFunctionSpider[A] {
  override def d = Map(Map("d" -> 1) -> coefficientRing.one)
  override def b = Map(Map("b" -> 1) -> coefficientRing.one)
}

trait IntegerMultivariableRationalFunctionTetravalentSpider extends MultivariableRationalFunctionTetravalentSpider[Fraction[BigInt]] with BigIntMultivariableRationalFunctionSpider

object TetravalentSpider extends IntegerMultivariableRationalFunctionTetravalentSpider with RationalFunctionPolyhedronNamer[Fraction[BigInt]]
