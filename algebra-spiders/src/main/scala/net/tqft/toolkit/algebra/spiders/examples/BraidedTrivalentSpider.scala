package net.tqft.toolkit.algebra.spiders.examples

import scala.language.reflectiveCalls
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction

abstract class BraidedTrivalentSpider[R: Field] extends PlanarGraphReductionSpiderOverField[R] {
  override lazy val vertexTypes = Seq(VertexType(3, 0, 1), VertexType(4, 0, 2))

  def d: R
  def b: R
  def t: R
  def z: R
  def omega: R
  override def eigenvalue(label: Int) = {
    label match {
      case 3 => omega
      case 4 => ring.one
    }
  }

  private lazy val loopReduction = Reduction(PlanarGraph.loop, Map(PlanarGraph.empty -> d))
  private lazy val monogonReduction = Reduction(PlanarGraph.polygon(1), Map(PlanarGraph.polygon(1) -> coefficients.zero))
  private lazy val bigonReduction = Reduction(PlanarGraph.polygon(2), Map(PlanarGraph.strand -> b))
  private lazy val triangleReduction = Reduction(PlanarGraph.polygon(3), Map(PlanarGraph.star(3) -> t))

   val vertex = PlanarGraph.star(3, 0, 1)
   val crossing = PlanarGraph.star(4, 0, 2)

   lazy val curlReduction1 = Reduction(diagramSpider.stitch(crossing), Map(PlanarGraph.strand -> ring.power(z, -2)))
   lazy val twistedVertexReduction1 = Reduction(diagramSpider.multiply(crossing, vertex, 2), Map(vertex -> ring.power(z,-1)))
   lazy val curlReduction2 = Reduction(diagramSpider.stitch(diagramSpider.rotate(crossing, 1)), Map(PlanarGraph.strand -> ring.power(z, 2)))
   lazy val twistedVertexReduction2 = Reduction(diagramSpider.multiply(diagramSpider.rotate(crossing, 1), vertex, 2), Map(vertex -> ring.power(z, 1)))
   lazy val Reidemeister2Reduction = Reduction(
    diagramSpider.multiply(crossing, diagramSpider.rotate(crossing, 1), 2),
    Map(PlanarGraph.two_strands_vertical -> ring.one))

  override def reductions = Seq(
    loopReduction,
    monogonReduction,
    bigonReduction,
    triangleReduction,
    curlReduction1,
    twistedVertexReduction1,
    curlReduction2,
    twistedVertexReduction2,
    Reidemeister2Reduction)

}

object BraidedTrivalentSpider extends BraidedTrivalentSpider[MultivariableRationalFunction[Fraction[BigInt], String]] with RationalFunctionPolyhedronNamer[Fraction[BigInt]] {
  override def coefficientRing = implicitly[Field[Fraction[BigInt]]]
  override def ring = implicitly[Field[MultivariableRationalFunction[Fraction[BigInt], String]]]

  override val omega = ring.one

  override val d: MultivariableRationalFunction[Fraction[BigInt], String] = Map(Map("d" -> 1) -> 1)
  override val b: MultivariableRationalFunction[Fraction[BigInt], String] = Map(Map("b" -> 1) -> 1)
  override val t: MultivariableRationalFunction[Fraction[BigInt], String] = Map(Map("t" -> 1) -> 1)
  override val z: MultivariableRationalFunction[Fraction[BigInt], String] = Map(Map("z" -> 1) -> 1)
}