package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction
import net.tqft.toolkit.algebra.Fraction

abstract class FreeSpider extends BigIntMultivariableRationalFunctionSpider with PolyhedronNamer[Fraction[BigInt]] {
  def generators: Seq[(VertexType, MultivariableRationalFunction[Fraction[BigInt], String])]
  override lazy val vertexTypes = generators.map(_._1)
  override def eigenvalue(label: Int): MultivariableRationalFunction[Fraction[BigInt], String] = {
    generators.filter(_._1.perimeter == label).ensuring(_.size == 1).head._2
  }
  override def reductions = Seq.empty
  
  override def toString = "FreeSpider(...)"
}
