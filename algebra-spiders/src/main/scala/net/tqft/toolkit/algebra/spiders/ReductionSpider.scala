package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices.Matrix

trait ReductionSpider[A, R] extends SubstitutionSpider[A, R] {
  def reductions: Seq[Reduction[A, R]]
  override def canonicalForm(m: Map[A, R]) = super.canonicalForm(replaceRepeatedly(reductions)(m))

}

abstract class PlanarGraphReductionSpider[R: GCDRing] extends SubstitutionSpider.PlanarGraphMapSubstitutionSpider[R] with ReductionSpider[PlanarGraph, R] {
  // move these further up the hierarchy?
  def innerProductMatrix(diagrams1: Seq[PlanarGraph], diagrams2: Seq[PlanarGraph]): Seq[Seq[R]] = {
    def ring = implicitly[Ring[R]]

    (for (x <- diagrams1) yield {
      (for (y <- diagrams2) yield {
        evaluatedInnerProduct(Map(x -> ring.one), Map(y -> ring.one))
      })
    })
  }
  def innerProductMatrix(diagrams: Seq[PlanarGraph]): Seq[Seq[R]] = innerProductMatrix(diagrams, diagrams)

  def reducedDiagrams(numberOfBoundaryPoints: Int, numberOfVertices: Int): Seq[PlanarGraph] = graphs.avoiding(reductions.map(_.big)).byNumberOfVertices(numberOfBoundaryPoints, numberOfVertices)
}

abstract class PlanarGraphReductionSpiderOverField[R: Field] extends PlanarGraphReductionSpider[R] { spider =>
  case class Basis(numberOfBoundaryPoints: Int, diagrams: Seq[PlanarGraph]) {
    def coefficients(x: PlanarGraph): Seq[R] = {
      val m = Matrix(diagrams.size + 1, innerProductMatrix(diagrams, x +: diagrams))
      m.nullSpace.head
    }
    def linearCombination(a: Seq[R]) = diagrams.zip(a).toMap
    def deriveNewRelations(numberOfVertices: Int): Iterator[Reduction[PlanarGraph, R]] = {
      require(numberOfVertices > diagrams.map(_.numberOfInternalVertices).max)
      for (big <- reducedDiagrams(numberOfBoundaryPoints, numberOfVertices).iterator) yield {
        Reduction(big, linearCombination(coefficients(big)))
      }
    }
    def deriveAllNewRelations: Stream[Reduction[PlanarGraph, R]] = {
      Stream.from(diagrams.map(_.numberOfInternalVertices).max + 1).flatMap(deriveNewRelations)
    }
    def withNewRelations(numberOfVertices: Int) = new PlanarGraphReductionSpiderOverField[R] {
      override def vertexTypes = spider.vertexTypes
      override def ring = spider.ring
      override def eigenvalue(valence: Int) = spider.eigenvalue(valence)
      override def reductions = spider.reductions ++ deriveNewRelations(numberOfVertices)
    }
  }

}