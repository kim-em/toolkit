package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.matrices.Matrices
import net.tqft.toolkit.algebra.polynomials.MultivariableRationalFunction

trait ReductionSpider[A, R] extends SubstitutionSpider[A, R] {
  def reductions: Seq[Reduction[A, R]]
  override def canonicalForm(m: Map[A, R]) = super.canonicalForm(replaceRepeatedly(reductions)(m))
  override def stitch(x: Map[A, R]): Map[A, R] = {
    replaceRepeatedly(reductions)(super.stitch(x))
  }
}


