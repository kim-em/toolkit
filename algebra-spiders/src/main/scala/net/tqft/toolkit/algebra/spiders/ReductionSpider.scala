package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.matrices.Matrices

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
        println("evaluating inner product of " + x + " and " + y)
        val r = evaluatedInnerProduct(Map(x -> ring.one), Map(y -> ring.one))
        println(r)
        r
      })
    })
  }
  def innerProductMatrix(diagrams: Seq[PlanarGraph]): Seq[Seq[R]] = innerProductMatrix(diagrams, diagrams)

  def reducedDiagrams(numberOfBoundaryPoints: Int, numberOfVertices: Int): Seq[PlanarGraph] = {
    require(vertexTypes.size == 1)
    reducedDiagrams(numberOfBoundaryPoints, Map(vertexTypes.head -> numberOfVertices))
  }
  def reducedDiagrams(numberOfBoundaryPoints: Int, numberOfVertices: Map[VertexType, Int]): Seq[PlanarGraph] = graphs.avoiding(reductions.map(_.big)).byNumberOfVertices(numberOfBoundaryPoints, numberOfVertices)
}

abstract class PlanarGraphReductionSpiderOverField[R: Field] extends PlanarGraphReductionSpider[R] { spider =>

  trait Basis {
    def numberOfBoundaryPoints: Int
    def diagrams: Seq[PlanarGraph]

    def coefficients(x: PlanarGraph): Seq[R] = {
      val m = Matrix(diagrams.size + 1, innerProductMatrix(diagrams, x +: diagrams))
      m.nullSpace.head
    }
    def linearCombination(a: Seq[R]) = diagrams.zip(a).toMap

    lazy val innerProducts = innerProductMatrix(diagrams)
    lazy val inverseInnerProducts = Matrix(diagrams.size, innerProducts).inverse.get.entries.seq

    lazy val actionOfRotation: Seq[Seq[R]] = {
      val m1 = Matrix(diagrams.size, innerProductMatrix(diagrams, diagrams.map(x => diagramSpider.rotate(x, 1))))
      val matrices = Matrices.matricesOver(diagrams.size)(ring)
      matrices.multiply(m1, Matrix(diagrams.size, inverseInnerProducts)).entries.seq
    }

    def verifyActionOfRotation = {
      // TODO check that the 2\pi rotation is the identity
    }

    def deriveNewRelations(numberOfVertices: Int): Iterator[Reduction[PlanarGraph, R]] = {
      require(vertexTypes.size == 1)
      deriveNewRelations(Map(vertexTypes.head -> numberOfVertices))
    }
    def deriveNewRelations(numberOfVertices: Map[VertexType, Int]): Iterator[Reduction[PlanarGraph, R]] = {
      for (big <- reducedDiagrams(numberOfBoundaryPoints, numberOfVertices).iterator) yield {
        Reduction(big, linearCombination(coefficients(big)))
      }
    }
    def withNewRelations(numberOfVertices: Int): PlanarGraphReductionSpiderOverField[R] = {
      require(vertexTypes.size == 1)
      withNewRelations(Map(vertexTypes.head -> numberOfVertices))
    }
    def withNewRelations(numberOfVertices: Map[VertexType, Int]): PlanarGraphReductionSpiderOverField[R] = new PlanarGraphReductionSpiderOverField[R] {
      override def vertexTypes = spider.vertexTypes
      override def ring = spider.ring
      override def eigenvalue(valence: Int) = spider.eigenvalue(valence)
      override def reductions = spider.reductions ++ deriveNewRelations(numberOfVertices)
    }
  }

  def basis(numberOfBoundaryPoints: Int, diagrams: Seq[PlanarGraph]): Basis = {
    val numberOfBoundaryPoints_ = numberOfBoundaryPoints
    val diagrams_ = diagrams
    new Basis {
      override val numberOfBoundaryPoints = numberOfBoundaryPoints_
      override val diagrams = diagrams_
    }

  }
}