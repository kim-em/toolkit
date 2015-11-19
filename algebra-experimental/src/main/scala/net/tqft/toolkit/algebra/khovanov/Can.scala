package net.tqft.toolkit.algebra.khovanov

import scala.language.implicitConversions
import net.tqft.toolkit.algebra.Fraction

case class Strand(arcs: Seq[Int])

case class Smoothing(strands: Seq[Strand], resolutions: Map[Int, Int])

object Smoothing {
  val empty = Smoothing(Seq.empty, Map.empty)
}

case class GradedSmoothing(q: Int, smoothing: Smoothing)

object GradedSmoothing {
  val empty = GradedSmoothing(0, Smoothing.empty)
}

case class Circle(arcs: Seq[Int])

case class Can(source: GradedSmoothing, target: GradedSmoothing, dots: Map[Circle, Boolean])

case class MonomialCan(can: Can, monomial: Map[String, Int]) // e.g. MonomialCan(can, Map("beta" -> 1)) represents a can multiplied by a formal variable beta

case class LinearComboCan(source: GradedSmoothing, target: GradedSmoothing, terms: Map[MonomialCan, Fraction[BigInt]])

object LinearComboCan {
  implicit def liftToLinearCombo(can: MonomialCan) = LinearComboCan(can.can.source, can.can.target, Map(can -> 1))
}
