package net.tqft.toolkit.algebra.spiders

import scala.language.reflectiveCalls

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.matrices.Matrices

abstract class PlanarGraphReductionSpiderOverField[R: Field] extends PlanarGraphReductionSpider[R] { spider =>
  override def ring = implicitly[Field[R]]

  trait CollectionOfDiagrams {
    def numberOfBoundaryPoints: Int
    def diagrams: Seq[PlanarGraph]

    lazy val innerProducts = innerProductMatrix(diagrams)
    lazy val determinant = Matrix(diagrams.size, innerProducts).determinant
    def linearCombination(a: Seq[R]) = diagrams.zip(a).toMap
  }

  trait LinearlyDependentDiagrams extends CollectionOfDiagrams {
    
  }

  trait LinearlyIndependentDiagrams extends CollectionOfDiagrams {
    lazy val inverseInnerProducts = Matrix(diagrams.size, innerProducts).inverse.get.entries.seq
    lazy val actionOfRotation: Seq[Seq[R]] = {
      val m1 = Matrix(diagrams.size, innerProductMatrix(diagrams, diagrams.map(x => diagramSpider.rotate(x, 1))))
      val matrices = Matrices.matricesOver(diagrams.size)(ring)
      matrices.multiply(m1, Matrix(diagrams.size, inverseInnerProducts)).entries.seq
    }

    def verifyActionOfRotation = {
      // TODO check that the 2\pi rotation is the identity
    }
  }

  trait SpanningDiagrams extends CollectionOfDiagrams {
    def coefficients(x: PlanarGraph): Seq[R] = {
      val additionalInnerProducts = for (d <- diagrams) yield evaluatedInnerProduct(Map(d -> ring.one), Map(x -> ring.one))
      val m = Matrix(diagrams.size + 1, innerProducts.zip(additionalInnerProducts).map(p => p._1 :+ p._2))
      val kernel = m.nullSpace.head
      require(kernel.last == ring.one)
      val result = kernel.most.map(ring.negate)
      require(result.size == diagrams.size)
      result
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
      override lazy val reductions = spider.reductions ++ deriveNewRelations(numberOfVertices)
    }
  }

  trait Basis extends LinearlyIndependentDiagrams with SpanningDiagrams {

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

trait FunctionSpider[A, F] extends PlanarGraphReductionSpider[F] {
  implicit def coefficientRing: Ring[A]
}

trait MultivariablePolynomialSpider[A] extends FunctionSpider[A, MultivariablePolynomial[A, String]] {
  override final lazy val ring = implicitly[Ring[MultivariablePolynomial[A, String]]]
}

trait BigIntMultivariablePolynomialSpider extends MultivariablePolynomialSpider[Fraction[BigInt]] {
  override final lazy val coefficientRing = implicitly[Field[Fraction[BigInt]]]
}
trait MultivariableRationalFunctionSpider[A] extends PlanarGraphReductionSpiderOverField[MultivariableRationalFunction[A, String]] with FunctionSpider[A, MultivariableRationalFunction[A, String]]  {
  override implicit def coefficientRing: Field[A]
  override final lazy val ring = implicitly[Field[MultivariableRationalFunction[A, String]]]
}

trait BigIntMultivariableRationalFunctionSpider extends MultivariableRationalFunctionSpider[Fraction[BigInt]] {
  override final lazy val coefficientRing = implicitly[Field[Fraction[BigInt]]]
}


