package net.tqft.toolkit.algebra.khovanov

import net.tqft.toolkit.algebra.Fraction

case class Strand(arcs: Seq[Int])

case class Smoothing(strands: Seq[Strand])

case class GradedSmoothing(q: Int, smoothing: Smoothing)

case class Circle(arcs: Seq[Int])

case class Can(bottom: GradedSmoothing, top: GradedSmoothing, dots: Map[Circle, Boolean])

case class MonomialCan(can: Can, beta: Int, gamma: Int)

case class Matrix[M](numberOfColumns: Int, entries: Seq[Seq[M]])

trait TensorCategory[O, M] {
  def compose(x: M, y: M): M
  def tensorObjects(x: O, y: O): O
  def tensorMorphisms(x: M, y: M): M
}

object TensorCategory {
  implicit class ComplexesOverTensorCategory[O, M](C: TensorCategory[O, M]) extends TensorCategory[Complex[O, M], ChainMap[O. M]] {
    
  }
}

case class Complex[O, M](
    homologicalHeight: Int, 
    objects: Seq[O],
    differentials: Seq[M])

case class ChainMap[O, M](
    source: Complex[O, M],
    target: Complex[O, M],
    components: Seq[M]
    ) 