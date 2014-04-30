package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.mathematica2.MathematicaForm
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.spiders.VertexType

object ExceptionalRelations extends App {
  // PlanarGraph(9,Vector(List(), List((4,12), (5,9), (8,11)), List((6,10), (8,12), (7,11)), List((4,9), (6,12), (7,10), (5,11))),ArrayBuffer(1, 1, 2),0)
  // PlanarGraph(7,Vector(List(), List((6,10), (3,8), (4,7), (5,9)), List((3,7), (6,8), (5,10), (4,9))),ArrayBuffer(2, 2),0)
  
  val basis = QuantumExceptionalSeries.basis(4, (QuantumExceptionalSeries.reducedDiagrams(4, Map.empty[VertexType, Int]) ++
    QuantumExceptionalSeries.reducedDiagrams(4, Map(VertexType(3, 1) -> 2)) ++
    QuantumExceptionalSeries.reducedDiagrams(4, Map(VertexType(4, 2) -> 1)).headOption).ensuring(_.size == 5))
  import MathematicaForm._
  implicit val mf = implicitly[MathematicaForm[Seq[MultivariableRationalFunction[BigInt, String]]]]
  for (r <- basis.innerProducts) {
    println(mf.toMathematicaInputString(r))
  }
}