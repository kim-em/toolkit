package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.mathematica.MathematicaForm
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial

abstract class FreeSpider extends BigIntMultivariableRationalFunctionSpider with RationalFunctionPolyhedronNamer[Fraction[BigInt]] {
  def generators: Seq[(VertexType, MultivariableRationalFunction[Fraction[BigInt], String])]
  override lazy val vertexTypes = generators.map(_._1)
  override def eigenvalue(label: Int): MultivariableRationalFunction[Fraction[BigInt], String] = {
    generators.filter(_._1.perimeter == label).ensuring(_.size == 1).head._2
  }
  override def reductions: Seq[Reduction[PlanarGraph, MultivariableRationalFunction[Fraction[BigInt], String]]] = polyhedronReductions

  override def toString = "FreeSpider(...)"
}

abstract class LowestWeightSpider extends FreeSpider {
  val lowestWeightReductions = for (
    (v, _) <- generators;
    k <- 0 until v.allowedRotationStep;
    d = diagramSpider.stitchAt(PlanarGraph.star(v.perimeter, v.allowedRotationStep), k)
  ) yield {
    //    println(s"Adding reduction formula so $v is lowest weight: $d")
    Reduction(d, Map.empty[PlanarGraph, MultivariableRationalFunction[Fraction[BigInt], String]])
  }

  override def reductions = super.reductions ++ lowestWeightReductions

  def asQuotientSpider = QuotientSpider(generators)

  override def toString = "LowestWeightSpider(...)"

}

case class QuotientSpider(
  generators: Seq[(VertexType, MultivariableRationalFunction[Fraction[BigInt], String])],
  extraReductions: Seq[Reduction[PlanarGraph, MultivariableRationalFunction[Fraction[BigInt], String]]] = Seq.empty) extends LowestWeightSpider {
  override def reductions = super.reductions ++ extraReductions

  def addReduction(reduction: Reduction[PlanarGraph, MultivariableRationalFunction[Fraction[BigInt], String]]) = {
    copy(extraReductions = extraReductions :+ reduction)
  }

  def reducibleDiagram_?(p: PlanarGraph): Boolean = {
    reductions.exists(r => p.Subgraphs(r.big).excisions.nonEmpty)
  }
  
  override def toString = {
    import MathematicaForm._
    val extraReductionsString = extraReductions.map({
      case Reduction(big, small) => s"Reduction($big, ${small.map(p => (p._1, p._2.toMathematicaInputString))})"
    }).toString
    s"QuotientSpider($generators, $extraReductionsString))"
  }
}

