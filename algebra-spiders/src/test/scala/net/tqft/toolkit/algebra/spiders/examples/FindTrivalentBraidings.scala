package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.spiders.PlanarGraph
import net.tqft.toolkit.algebra.spiders.TrivalentGraphs
import net.tqft.toolkit.algebra.mathematica.MathematicaForm
import net.tqft.toolkit.algebra.Fraction

object FindTrivalentBraidings extends App {

//  val SO3equations = TrivalentBraidings.prepareBraidingEquationInnerProducts[MultivariableRationalFunction[BigInt, String]](
//    `SO(3)_q`,
//    Map(PlanarGraph.star(3) -> 1),
//    TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(4, 0).take(3).zipWithIndex.map(p => (p._1, (("v" + p._2): MultivariablePolynomial[BigInt, String]): MultivariableRationalFunction[BigInt, String])).toMap,
//    TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(4, 0).map(d => Map(d -> (1: MultivariableRationalFunction[BigInt, String]))),
//    TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(5, 1).map(d => Map(d -> (1: MultivariableRationalFunction[BigInt, String]))))
    
  val cubicEquations = TrivalentBraidings.prepareBraidingEquationInnerProducts[MultivariableRationalFunction[Fraction[BigInt], String]](
    CubicSpider,
    Map(PlanarGraph.star(3) -> 1),
    TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(4, 0).zipWithIndex.map(p => (p._1, (("v" + p._2): MultivariablePolynomial[Fraction[BigInt], String]): MultivariableRationalFunction[Fraction[BigInt], String])).toMap,
    TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(4, 0).map(d => Map(d -> (1: MultivariableRationalFunction[Fraction[BigInt], String]))),
    TrivalentGraphs.withoutSmallFaces.withAtMostNumberOfFaces(5, 1).map(d => Map(d -> (1: MultivariableRationalFunction[Fraction[BigInt], String]))))
    
    import MathematicaForm._
//    println(SO3equations.map(_.toMathematicaInputString).mkString("{\n", ", \n","\n}"))
    println(cubicEquations.map(_.toMathematicaInputString).mkString("{\n", ", \n","\n}"))
}