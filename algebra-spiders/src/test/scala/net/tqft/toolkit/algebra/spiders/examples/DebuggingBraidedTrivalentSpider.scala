package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.mathematica.MathematicaForm._
import net.tqft.toolkit.algebra.mathematica.MathematicaForm

object DebuggingBraidedTrivalentSpider extends App {
  val bts = BraidedTrivalentSpider
  val one = bts.ring.one

  //  println(bts.canonicalForm(Map(PlanarGraph.hopfStrand -> one)))
  //  println(
  //      BraidedTrivalentSpider.innerProduct(Map(PlanarGraph.hopfStrand -> one), Map(PlanarGraph.hopfStrand -> one))
  //      )


    val diagrams = Seq(PlanarGraph.I, PlanarGraph.H, bts.crossing)
    println(bts.innerProductMatrix(diagrams, diagrams).map(_.map(_.toMathematicaInputString)).mkString("\n"))
}