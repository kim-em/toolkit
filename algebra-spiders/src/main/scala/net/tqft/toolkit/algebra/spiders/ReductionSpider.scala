package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.Ring

trait ReductionSpider[A, R] extends SubstitutionSpider[A, R] {
  def reductions: Seq[Reduction[A, R]]
  override def canonicalForm(m: Map[A, R]) = super.canonicalForm(replaceRepeatedly(reductions)(m))

}

abstract class PlanarGraphReductionSpider[R: Ring] extends SubstitutionSpider.PlanarGraphMapSubstitutionSpider[R] with ReductionSpider[PlanarGraph, R] {
  // move these further up the hierarchy?
  def innerProductMatrix(diagrams1: IndexedSeq[PlanarGraph], diagrams2: IndexedSeq[PlanarGraph]): IndexedSeq[IndexedSeq[R]] = {
    def ring = implicitly[Ring[R]]

    for (x <- diagrams1) yield {
      for (y <- diagrams2) yield {
        evaluatedInnerProduct(Map(x -> ring.one), Map(y -> ring.one))
      }
    }
  }
  def innerProductMatrix(diagrams: IndexedSeq[PlanarGraph]): IndexedSeq[IndexedSeq[R]] = innerProductMatrix(diagrams, diagrams)

}
