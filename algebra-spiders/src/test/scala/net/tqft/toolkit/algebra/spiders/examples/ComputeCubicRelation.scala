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

  val m = Matrix(5, CubicSpider.innerProductMatrix(`D(4,0)`, `D(4,1)`))

  import MathematicaForm._

  println("The cubic relation: ")

  println("matrix: ")
  println(m.entries.toIndexedSeq.toMathematicaInputString)
  println("nullspace: ")
  println(m.nullSpace.toMathematicaInputString)

  println("The relations in the twisted cubic category: ")

  val m2 = Matrix(5, TwistedCubicSpider.innerProductMatrix(`D(4,0)`, `D(4,1)`))
  implicit val polynomialForm = MathematicaForm.polynomialMathematicaForm[Fraction[BigInt]]("w")

  for (r <- TwistedCubicSpider.reductions) {
    println(r.big)
    for ((d, x) <- r.small) {
      println(d)
      println(x.toMathematicaInputString)
    }
  }

  println("matrix: ")
  println(m2.entries.toIndexedSeq.toMathematicaInputString)
  println("nullspace: ")
  println(m2.nullSpace(TwistedCubicSpider.ring).toMathematicaInputString)
}