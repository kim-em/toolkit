package net.tqft.toolkit.algebra.khovanov

import scala.language.implicitConversions
import net.tqft.toolkit.algebra.Fraction
import scala.collection.mutable.Buffer
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.AdditiveMonoid

case class Strand(arcs: Seq[Int])

case class Smoothing(strands: Seq[Strand])

case class GradedSmoothing(q: Int, smoothing: Smoothing)

case class Circle(arcs: Seq[Int])

case class Can(source: GradedSmoothing, target: GradedSmoothing, dots: Map[Circle, Boolean])

object Can {
  def identity(s: GradedSmoothing): Can = ???
}

case class MonomialCan(can: Can, beta: Int, gamma: Int)

case class LinearComboCan(source: GradedSmoothing, target: GradedSmoothing, terms: Map[MonomialCan, Fraction[BigInt]])

object LinearComboCan {
  implicit def liftToLinearCombo(can: MonomialCan) = LinearComboCan(can.can.source, can.can.target, Map(can -> 1))
}

object SurfacesModRelations extends TensorCategory[GradedSmoothing, LinearComboCan] {
  val mapMonoid = implicitly[AdditiveMonoid[Map[MonomialCan, Fraction[BigInt]]]]

  override def source(m: LinearComboCan) = m.source
  override def target(m: LinearComboCan) = m.target
  override def zero(x: GradedSmoothing, y: GradedSmoothing) = LinearComboCan(x, y, Map.empty)
  override def identity(x: GradedSmoothing) = MonomialCan(Can.identity(x), 0, 0)
  override def add(x: LinearComboCan, y: LinearComboCan) = {
    require(x.source == y.source)
    require(x.target == y.target)
    LinearComboCan(x.source, x.target, mapMonoid.add(x.terms, y.terms))
  }

  private def composeCans(x: Can, y: Can): LinearComboCan = ???
  private def composeMonomialCans(x: MonomialCan, y: MonomialCan) = ???

  override def compose(x: LinearComboCan, y: LinearComboCan) = {
    require(x.target == y.source)
    LinearComboCan(x.source, y.target, ???)
  }

  override def tensorObjects(x: GradedSmoothing, y: GradedSmoothing) = ???

  private def tensorCans(x: Can, y: Can): LinearComboCan = ???
  private def tensorMonomialCans(x: MonomialCan, y: MonomialCan) = ???

  override def tensorMorphisms(x: LinearComboCan, y: LinearComboCan) = {
    LinearComboCan(
      tensorObjects(x.source, y.source),
      tensorObjects(x.target, y.target),
      ???)
  }
}

object SurfacesGaussianEliminator extends GaussianElimination[GradedSmoothing, LinearComboCan] {
  override def locatePseudoIsomorphism(m: Matrix[GradedSmoothing, LinearComboCan]) = ???
  override def pseudoInverse(can: LinearComboCan) = {
    require(can.terms.size == 1)
    require(can.source == can.target)
    val mCan = can.terms.head._1
    val rationals = implicitly[Field[Fraction[BigInt]]]
    LinearComboCan(can.source, can.source, Map(MonomialCan(mCan.can, -mCan.beta, -mCan.gamma) -> rationals.inverse(can.terms.head._2)))
  }
  override def invertible_?(can: LinearComboCan) = {
    val mCan = can.terms.head._1
    mCan.beta == 0 && mCan.gamma == 0
  }
}

