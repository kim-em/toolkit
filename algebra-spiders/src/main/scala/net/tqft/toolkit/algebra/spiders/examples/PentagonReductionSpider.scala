package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.spiders._

abstract class PentagonReductionSpider[R: Field] extends CubicSpider[R] { cs =>
  private lazy val pentagonReduction: Reduction[PlanarGraph, R] = {
    object PrePentagonReductionSpider extends CubicSpider[R] {
      override def ring = cs.ring
      override def omega = cs.omega
      override def d = cs.d
      override def b = cs.b
      override def t = cs.t
    }
    PrePentagonReductionSpider.basis(5, PrePentagonReductionSpider.reducedDiagrams(5, 1) ++ PrePentagonReductionSpider.reducedDiagrams(5, 3)).deriveNewRelations(5).next
  }
  override def reductions = super.reductions :+ pentagonReduction
}
