package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.numberfields.NumberField
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.matrices.Matrices

abstract class TrivalentSpider[R: Field] extends PlanarGraphReductionSpiderOverField[R] {
  override lazy val vertexTypes = Seq(VertexType(3, 1))

  def d: R
  def b: R
  def t: R
  def omega: R
  override def eigenvalue(label: Int) = {
    label match {
      case 3 => omega
    }
  }

  private lazy val loopReduction = Reduction(PlanarGraph.loop, Map(PlanarGraph.empty -> d))
  private lazy val monogonReduction = Reduction(PlanarGraph.polygon(1), Map(PlanarGraph.polygon(1) -> coefficients.zero))
  private lazy val bigonReduction = Reduction(PlanarGraph.polygon(2), Map(PlanarGraph.strand -> b))
  private lazy val triangleReduction = Reduction(PlanarGraph.polygon(3), Map(PlanarGraph.star(3) -> t))
  override def reductions = Seq(loopReduction, monogonReduction, bigonReduction, triangleReduction)
}

trait MultivariableRationalFunctionTrivalentSpider[A] extends TrivalentSpider[MultivariableRationalFunction[A, String]] {
  implicit def coefficientRing: EuclideanRing[A]
  override def ring = implicitly[Field[MultivariableRationalFunction[A, String]]]
  override def d = Map(Map("d" -> 1) -> coefficientRing.one)
  override def b = Map(Map("b" -> 1) -> coefficientRing.one)
  override def t = Map(Map("t" -> 1) -> coefficientRing.one)
}

trait IntegerMultivariableRationalFunctionTrivalentSpider extends MultivariableRationalFunctionTrivalentSpider[BigInt] {
  override def omega = 1
  override def coefficientRing = implicitly[EuclideanRing[BigInt]]
}
trait TwistedMultivariableRationalFunctionTrivalentSpider extends MultivariableRationalFunctionTrivalentSpider[Polynomial[Fraction[Int]]] {
  override def t = ring.zero
  // TODO improve implicits here?:
  override def omega = MultivariablePolynomial.constant[Polynomial[Fraction[Int]], String](Polynomial[Fraction[Int]](Map(1 -> Fraction.whole(1))))
  override def coefficientRing: Field[Polynomial[Fraction[Int]]] = NumberField.cyclotomic[Fraction[Int]](3)
}

object TrivalentSpider extends IntegerMultivariableRationalFunctionTrivalentSpider

object TwistedTrivalentSpider extends TwistedMultivariableRationalFunctionTrivalentSpider

abstract class CubicSpider[R: Field] extends TrivalentSpider[R] { cs =>
  private lazy val squareReduction: Reduction[PlanarGraph, R] = {
    object PreCubicSpider extends TrivalentSpider[R] {
      override def ring = cs.ring
      override def omega = cs.omega
      override def d = cs.d
      override def b = cs.b
      override def t = cs.t
    }
    PreCubicSpider.Basis(4, PreCubicSpider.reducedDiagrams(4, 0) ++ PreCubicSpider.reducedDiagrams(4, 2)).deriveNewRelations(4).next
  }
  override def reductions = super.reductions :+ squareReduction
}

object Spiders {
  def cubic[R: Field](omega: R, d: R, b: R, t: R) = {
    val omega_ = omega
    val d_ = d
    val b_ = b
    val t_ = t
    val preCubicSpider = new TrivalentSpider[R] {
      override def ring = implicitly[Field[R]]
      override def omega = omega_
      override def d = d_
      override def b = b_
      override def t = t_
    }
    preCubicSpider.Basis(4, preCubicSpider.reducedDiagrams(4, 0) ++ preCubicSpider.reducedDiagrams(4, 2)).withNewRelations(4)
  }
}

object CubicSpider extends CubicSpider[MultivariableRationalFunction[BigInt, String]] with IntegerMultivariableRationalFunctionTrivalentSpider {
  override val omega = ring.one

  //  def z: MultivariableRationalFunction[BigInt, String] = Map(Map("z" -> 1) -> 1) // inverse of MultivariablePolynomial(Map(Map("b" -> 1, "d" -> 1) -> 1, Map("d" -> 1, "t" -> 1) -> 1, Map("t" -> 1) -> 1)) // b d + t + d t

  //  override def alpha = ring.multiply(z, Map(Map("b" -> 3) -> 1, Map("b" -> 2, "t" -> 1) -> 1, Map("b" -> 1, "t" -> 2) -> -1)) // b^3 + b^2 t - b t^2
  //  override def beta = ring.multiply(z, Map(Map("b" -> 2) -> -1, Map("t" -> 2) -> 1, Map("d" -> 1, "t" -> 2) -> 1)) // -b^2 + t^2 + d t^2
}

object TwistedCubicSpider extends CubicSpider[MultivariableRationalFunction[Polynomial[Fraction[Int]], String]] with TwistedMultivariableRationalFunctionTrivalentSpider {
  //  def `d^(-1)`: MultivariableRationalFunction[Polynomial[Fraction[Int]], String] = Map(Map("(d^(-1))" -> 1) -> 1)
  //  override def alpha = ring.multiply(`d^(-1)`, Map(Map("b" -> 2) -> 1))
  //  override def beta = ring.multiply(`d^(-1)`, Map(Map("b" -> 1) -> -1))
}

abstract class PentagonReductionSpider[R: Field] extends CubicSpider[R] { cs =>
  private lazy val pentagonReduction: Reduction[PlanarGraph, R] = {
    object PrePentagonReductionSpider extends CubicSpider[R] {
      override def ring = cs.ring
      override def omega = cs.omega
      override def d = cs.d
      override def b = cs.b
      override def t = cs.t
    }
    PrePentagonReductionSpider.Basis(5, PrePentagonReductionSpider.reducedDiagrams(5, 1) ++ PrePentagonReductionSpider.reducedDiagrams(5, 3)).deriveNewRelations(5).next
  }
  override def reductions = super.reductions :+ pentagonReduction
}

object `SO(3)_q` extends PentagonReductionSpider[RationalFunction[Int]] {
  override val ring = implicitly[Field[RationalFunction[Int]]]

  val q: RationalFunction[Int] = Polynomial(1 -> 1)
  override val d = ring.add(q, ring.inverse(q))
  override val b = ???
  override val t = ???
  override val omega = ring.one
}

object `(G_2)_q` extends PentagonReductionSpider[RationalFunction[Int]] {
  override val ring = implicitly[Field[RationalFunction[Int]]]

  override val d: RationalFunction[Int] = Map(5 -> 1, 4 -> 1, 1 -> 1, 0 -> 1, -1 -> 1, -4 -> 1, -5 -> 1)
  override val b: RationalFunction[Int] = Map(3 -> 1, 2 -> 1, 1 -> 1, -1 -> 1, -2 -> 1, -3 -> 1)
  override val t: RationalFunction[Int] = Map(2 -> -1, 0 -> -1, -2 -> -1)
  override val omega = ring.one
}

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

  private lazy val curlReduction = Reduction(diagramSpider.stitch(crossing), Map(PlanarGraph.strand -> ring.power(z, 2)))
  private lazy val twistedVertexReduction = Reduction(diagramSpider.multiply(crossing, vertex, 2), Map(vertex -> z))
  private lazy val Reidemeister2Reduction = Reduction(
    diagramSpider.multiply(crossing, diagramSpider.rotate(crossing, 1), 2),
    Map(PlanarGraph.two_strands_vertical -> ring.one))

  override def reductions = Seq(
    loopReduction,
    monogonReduction,
    bigonReduction,
    triangleReduction,
    curlReduction,
    twistedVertexReduction,
    Reidemeister2Reduction)

  def actionOfBraiding(basis: Seq[PlanarGraph]) = {
    val m1 = Matrix(basis.size, innerProductMatrix(basis, basis.map(x => diagramSpider.multiply(x, crossing, 2))))
    val m2 = Matrix(basis.size, innerProductMatrix(basis))
    val matrices = Matrices.matricesOver(basis.size)(ring)
    matrices.multiply(m1, m2.inverse.get).entries.seq
  }

  def verifyActionOfBraiding(basis: Seq[PlanarGraph]) = {
    val s1 = Matrix(basis.size, actionOfBraiding(basis))
    val matrices = Matrices.matricesOver(basis.size)(ring)
    val rho = Matrix(basis.size, actionOfRotation(basis))
    val s2 = matrices.multiply(rho.inverse.get, s1, rho)

    matrices.multiply(s1, s2, s1) == matrices.multiply(s2, s1, s2)
  }
}

object QuantumExceptional extends BraidedTrivalentSpider[MultivariableRationalFunction[BigInt, String]] {
  override def ring = implicitly[Field[MultivariableRationalFunction[BigInt, String]]]

  override val omega = ring.one

  val v: MultivariableRationalFunction[BigInt, String] = Map(Map("v" -> 1) -> 1)
  val w: MultivariableRationalFunction[BigInt, String] = Map(Map("w" -> 1) -> 1)

  import AlgebraicNotation._

  def bracket(k: Int, l: Int) = (w ^ k) * (v ^ k) - (w ^ (-l)) * (v ^ (-k))
  def brace(k: Int, l: Int) = (w ^ k) * (v ^ k) + (w ^ (-l)) * (v ^ (-k))

  override val d = -((brace(0, 2) * bracket(1, 5) * bracket(1, -6)) / (bracket(1, 0) * bracket(1, -1)))
  override val b = (brace(1, 2) * brace(1, -3) * bracket(0, 3)) / bracket(0, 1)
  override val t = brace(0, 1) * ((v + (v ^ (-1))) * ((w ^ 2) * (v ^ (-1)) + (w ^ (-2)) * v)) + ((v ^ 4) - (v ^ 2) - 1 - (v ^ (-2)) + (v ^ (-4)))
  override def z = -(v ^ 6)

  override def reductions = super.reductions

  lazy val basisFor3Boxes =
    Seq(vertex)

  lazy val basisFor4Boxes =
    (reducedDiagrams(4, Map.empty[VertexType, Int]) ++
      reducedDiagrams(4, Map(VertexType(3, 1) -> 2)) ++
      reducedDiagrams(4, Map(VertexType(4, 2) -> 1)).headOption).ensuring(_.size == 5)

  lazy val basisFor5Boxes = {
    val tangle = reducedDiagrams(5, Map(VertexType(3, 1) -> 3, VertexType(4, 2) -> 1)).head
    val tangles = Seq.tabulate(5)(k => diagramSpider.rotate(tangle, k))

    (reducedDiagrams(5, Map(VertexType(3, 1) -> 1)) ++
      reducedDiagrams(5, Map(VertexType(3, 1) -> 3)) ++
      reducedDiagrams(5, Map(VertexType(3, 1) -> 5)) ++ tangles).ensuring(_.size == 16)
  }

  lazy val basisFor6Boxes = ???
}
