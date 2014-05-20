package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.matrices._

abstract class BraidedTrivalentSpider[R: Field] extends PlanarGraphReductionSpiderOverField[R] {
  override lazy val vertexTypes = Seq(VertexType(3, 1), VertexType(4, 2))

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

  protected val vertex = PlanarGraph.star(3, 1)
  protected val crossing = PlanarGraph.star(4, 2)

  private lazy val curlReduction1 = Reduction(diagramSpider.stitch(crossing), Map(PlanarGraph.strand -> ring.power(z, 2)))
  private lazy val twistedVertexReduction1 = Reduction(diagramSpider.multiply(crossing, vertex, 2), Map(vertex -> z))
  private lazy val curlReduction2 = Reduction(diagramSpider.stitch(diagramSpider.rotate(crossing, 1)), Map(PlanarGraph.strand -> ring.power(z, 2)))
  private lazy val twistedVertexReduction2 = Reduction(diagramSpider.multiply(diagramSpider.rotate(crossing, 1), vertex, 2), Map(vertex -> z))
  private lazy val Reidemeister2Reduction = Reduction(
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

  def actionOfBraiding(basis: Seq[PlanarGraph]) = {
    val m1 = Matrix(basis.size, innerProductMatrix(basis, basis.map(x => diagramSpider.multiply(x, crossing, 2))))
    val m2 = Matrix(basis.size, innerProductMatrix(basis))
    val matrices = Matrices.matricesOver(basis.size)(ring)
    matrices.multiply(m1, m2.inverse.get).entries.seq
  }

  case class Braid(width: Int, crossings: Seq[(Int, Int)])

  trait Basis extends super.Basis {
    lazy val actionOfBraiding = {
      val m1 = Matrix(diagrams.size, innerProductMatrix(diagrams, diagrams.map(x => diagramSpider.multiply(x, crossing, 2))))
      val matrices = Matrices.matricesOver(diagrams.size)(ring)
      matrices.multiply(m1, Matrix(diagrams.size, inverseInnerProducts)).entries.seq
    }
    def verifyActionOfBraiding = {
      val s1 = Matrix(diagrams.size, actionOfBraiding)
      val matrices = Matrices.matricesOver(diagrams.size)(ring)
      val rho = Matrix(diagrams.size, actionOfRotation)
      val s2 = matrices.multiply(rho.inverse.get, s1, rho)

      matrices.multiply(s1, s2, s1) == matrices.multiply(s2, s1, s2)
    }
  }

  trait BasisWithPlatElement extends Basis {
    def platElement: Int

    private lazy val braidCache = {
      def braidElement(crossing: (Int, Int)) = ???
      ???
    }

    def braidActionOnPlatElement(braid: Braid): Seq[R] = {
      ???
    }

    def platClosure(braid: Braid): R = {
      ring.product(
        braidActionOnPlatElement(braid)
          .zip(innerProducts.map(_(platElement)))
          .map(p => ring.multiply(p._1, p._2)))
    }
  }

  override def basis(numberOfBoundaryPoints: Int, diagrams: Seq[PlanarGraph]): Basis = {
    val numberOfBoundaryPoints_ = numberOfBoundaryPoints
    val diagrams_ = diagrams
    new Basis {
      override val numberOfBoundaryPoints = numberOfBoundaryPoints_
      override val diagrams = diagrams_
    }
  }
  def basisWithPlatElement(numberOfBoundaryPoints: Int, diagrams: Seq[PlanarGraph]): BasisWithPlatElement = {
    require(numberOfBoundaryPoints % 2 == 0)
    val numberOfBoundaryPoints_ = numberOfBoundaryPoints
    val diagrams_ = diagrams
    new BasisWithPlatElement {
      override val numberOfBoundaryPoints = numberOfBoundaryPoints_
      override val diagrams = diagrams_
      override val platElement = diagrams.indexWhere({ d =>
        diagramSpider.canonicalFormWithDefect(d) == diagramSpider.canonicalFormWithDefect(diagramSpider.tensorProduct(Seq.fill(numberOfBoundaryPoints / 2)(PlanarGraph.strand)))
      }).ensuring(_ != -1)
    }
  }

}