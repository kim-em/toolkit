package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.mathematica._
import net.tqft.toolkit.algebra.mathematica.MathematicaForm

object ComputeCubicRelation extends App {
  lazy val `D(4,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(4, 0).toList
  lazy val `D(4,1)` = TrivalentGraphs.withoutTinyFaces.byNumberOfFaces(4, 1).toList

  val m = Matrix(5, CubicSpider.innerProductMatrix(`D(4,0)`, `D(4,1)`).map(_.map(x => (x: MultivariableRationalFunction[BigInt, String]))))

  import MathematicaForm._

  println("matrix: ")
  println(m.entries.toIndexedSeq.toMathemathicaInputString)
  println("nullspace: ")
  println(m.nullSpace.toMathemathicaInputString)
}