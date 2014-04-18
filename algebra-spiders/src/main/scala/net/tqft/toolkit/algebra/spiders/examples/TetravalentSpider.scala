package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.numberfields.NumberField

abstract class TetravalentSpider[R: Field] extends PlanarGraphReductionSpiderOverField[R] {
  override lazy val vertexTypes = Seq(VertexType(4, 1))

  def d: R
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
  override def reductions = Seq(loopReduction, uncappableReduction)
}

trait MultivariableRationalFunctionTetravalentSpider[A] extends TetravalentSpider[MultivariableRationalFunction[A, String]] with MultivariableRationalFunctionSpider[A] {
  override def d = Map(Map("d" -> 1) -> coefficientRing.one)
}

trait IntegerMultivariableRationalFunctionTetravalentSpider extends MultivariableRationalFunctionTetravalentSpider[BigInt] {
  override def coefficientRing = implicitly[EuclideanRing[BigInt]]
}

object TetravalentSpider extends IntegerMultivariableRationalFunctionTetravalentSpider with PolyhedronNamer[BigInt]
