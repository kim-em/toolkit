package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.mathematica._
import net.tqft.toolkit.algebra.mathematica.MathematicaForm

object ComputeInnerProducts extends App {
  lazy val `D(4,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(4, 0).toList
  lazy val `D(4,1)` = TrivalentGraphs.withoutTinyFaces.byNumberOfFaces(4, 1).toList
  def D(n: Int, k: Int) = {
    TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(n, k).toList
  }
  def M(n: Int, k: Int) = {
    CubicSpider.innerProductMatrix(D(n, k))
  }
  def Mw(n: Int, k: Int) = {
    TwistedCubicSpider.innerProductMatrix(D(n, k))
  }

  import mathematica.MathematicaForm._
  implicit val polynomialForm = MathematicaForm.polynomialMathematicaForm[Fraction[Int]]("w")

  for ((n, k) <- Seq(/*(4, 0), (4, 1), (5, 0), (5, 1), (6, 0), (6, 1), (6, 2), *//*(7, 0), (7,1), (7,2)*/(8,0), (8,1), (9,0))) {
    println(s"twisted M($n, $k)")
    println(Mw(n, k).toMathematicaInputString)
  }
}