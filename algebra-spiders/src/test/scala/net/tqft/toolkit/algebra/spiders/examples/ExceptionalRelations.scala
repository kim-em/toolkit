package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.mathematica.MathematicaForm
import net.tqft.toolkit.algebra.polynomials.{ RationalExpression => MultivariableRationalFunction }
import net.tqft.toolkit.algebra.spiders.VertexType
import net.tqft.toolkit.algebra.Fraction

object ExceptionalRelations extends App {
  // PlanarGraph(9,Vector(List(), List((4,12), (5,9), (8,11)), List((6,10), (8,12), (7,11)), List((4,9), (6,12), (7,10), (5,11))),ArrayBuffer(1, 1, 2),0)
  // PlanarGraph(7,Vector(List(), List((6,10), (3,8), (4,7), (5,9)), List((3,7), (6,8), (5,10), (4,9))),ArrayBuffer(2, 2),0)
  
  val basis = QuantumExceptionalSeries.basis(4, (QuantumExceptionalSeries.reducedDiagrams(4, Map.empty[VertexType, Int]) ++
    QuantumExceptionalSeries.reducedDiagrams(4, Map(VertexType(3, 0, 1) -> 2)) ++
    QuantumExceptionalSeries.reducedDiagrams(4, Map(VertexType(4, 0, 2) -> 1)).headOption).ensuring(_.size == 5))
  import MathematicaForm._
  implicit val mf = implicitly[MathematicaForm[Seq[MultivariableRationalFunction[Fraction[BigInt], String]]]]
  for (r <- basis.innerProducts) {
    println(mf.toMathematicaInputString(r))
  }
}