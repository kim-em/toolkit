package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials.{ RationalExpression => MultivariableRationalFunction }
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.numberfields.NumberField

abstract class TrivalentSpider[R: Field] extends PlanarGraphReductionSpiderOverField[R] {
  override lazy val vertexTypes = Seq(VertexType(3, 0, 1))

  def d: R
  def b: R
  def t: R
  def omega: R
  override def eigenvalue(vt: VertexType) = {
    vt.perimeter match {
      case 3 => omega
    }
  }

  private lazy val loopReduction = Reduction(PlanarGraph.loop, Map(PlanarGraph.empty -> d))
  private lazy val monogonReduction = Reduction(PlanarGraph.polygon(1), Map(PlanarGraph.polygon(1) -> coefficients.zero))
  private lazy val bigonReduction = Reduction(PlanarGraph.polygon(2), Map(PlanarGraph.strand -> b))
  private lazy val triangleReduction = Reduction(PlanarGraph.polygon(3), Map(PlanarGraph.star(3) -> t))
  override def reductions = Seq(loopReduction, monogonReduction, bigonReduction, triangleReduction)

  def rectangularLowestWeightConditions(spanningSet1: Seq[PlanarGraph], spanningSet2: Seq[PlanarGraph])(d: PlanarGraph): Seq[R] = {
    val caps = for (k <- 1 until d.numberOfBoundaryPoints; if k != d.numberOfBoundaryPoints / 2) yield {
      DiagramSpider.graphSpider.stitchAt(d, k)
    }
    val fuses = for (k <- 1 until d.numberOfBoundaryPoints; if k != d.numberOfBoundaryPoints / 2) yield {
      DiagramSpider.graphSpider.multiply(PlanarGraph.star(3), DiagramSpider.graphSpider.rotate(d, 1 - k), 2)
    }
    innerProductMatrix(fuses, spanningSet1).flatten ++ innerProductMatrix(caps, spanningSet2).flatten
    
    // TODO test this, and try it on the Cantor category.
  }
}

//trait MultivariableRationalFunctionTrivalentSpider[A] extends TrivalentSpider[MultivariableRationalFunction[A, String]] with MultivariableRationalFunctionSpider[A] {
//  override def d = Map(Map("d" -> 1) -> coefficientRing.one)
//  override def b = Map(Map("b" -> 1) -> coefficientRing.one)
//  override def t = Map(Map("t" -> 1) -> coefficientRing.one)
//}
//
//trait IntegerMultivariableRationalFunctionTrivalentSpider extends MultivariableRationalFunctionTrivalentSpider[Fraction[BigInt]] {
//  override def omega = 1
//  override final lazy val coefficientRing = implicitly[Field[Fraction[BigInt]]]
//}
//trait TwistedMultivariableRationalFunctionTrivalentSpider extends MultivariableRationalFunctionTrivalentSpider[Polynomial[Fraction[BigInt]]] {
//  override def t = ring.zero
//  // TODO improve implicits here?:
//  override def omega = MultivariablePolynomial.constant[Polynomial[Fraction[BigInt]], String](Polynomial[Fraction[BigInt]](Map(1 -> Fraction.whole[BigInt](1))))
//  override final lazy val coefficientRing: Field[Polynomial[Fraction[BigInt]]] = NumberField.cyclotomic[BigInt](3)
//}
//
//object TrivalentSpider extends IntegerMultivariableRationalFunctionTrivalentSpider
//
//object TwistedTrivalentSpider extends TwistedMultivariableRationalFunctionTrivalentSpider
