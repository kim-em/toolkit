package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.mathematica.MathematicaForm
import net.tqft.toolkit.algebra.matrices.Matrix

object ComputeInnerProducts extends App {
  lazy val `D(4,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(4, 0).toList
  lazy val `D(4,1)` = TrivalentGraphs.withoutTinyFaces.byNumberOfFaces(4, 1).toList
  lazy val `D(5,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(5, 0)
  lazy val `D(6,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(6, 0)

//  {
//    val spider = Trivalent.TrivalentSpider
//
//    val `M(4,0)` = spider.innerProductMatrix(`D(4,0)`)
//
//    import net.tqft.toolkit.algebra.mathematica.MathematicaForm._
//
//    println("trivalent Delta(4,0)")
//    println(`M(4,0)`.toMathemathicaInputString)
//
//    println(spider.innerProductMatrix(`D(4,0)`, `D(4,1)`).toMathemathicaInputString)
//  }
  //  {
  //    val spider = Trivalent.TrivalentSpider
  //
  //    val `M(5,0)` = spider.innerProductMatrix(`D(5,0)`.toIndexedSeq)
  //
  //    import net.tqft.toolkit.algebra.mathematica.MathematicaForm._
  //
  //    println("trivalent Delta(5,0)")
  //    println(`M(5,0)`.toMathemathicaInputString)
  //  }
  //  {
  //    val spider = Trivalent.TrivalentSpider
  //
  //    val `M(6,0)` = spider.innerProductMatrix(`D(6,0)`.toIndexedSeq)
  //
  //    import net.tqft.toolkit.algebra.mathematica.MathematicaForm._
  //
  //    println("trivalent Delta(6,0)")
  //    println(`M(6,0)`.toMathemathicaInputString)
  //  }
  //
  {
    val spider = TwistedTrivalent.TwistedTrivalentSpider

    val `M(4,0)` = spider.innerProductMatrix(`D(4,0)`.toIndexedSeq)

    import net.tqft.toolkit.algebra.mathematica.MathematicaForm._
    implicit val polynomialForm = MathematicaForm.polynomialMathematicaForm[Fraction[Int]]("w")

    println("twisted trivalent Delta(4,0)")
    println(`M(4,0)`.toMathemathicaInputString)

    println(spider.innerProductMatrix(`D(4,0)`, `D(4,1)`).toMathemathicaInputString)
  }
  {
    val spider = TwistedTrivalent.TwistedTrivalentSpider

    val `M(5,0)` = spider.innerProductMatrix(`D(5,0)`.toIndexedSeq)

    import net.tqft.toolkit.algebra.mathematica.MathematicaForm._
    implicit val polynomialForm = MathematicaForm.polynomialMathematicaForm[Fraction[Int]]("w")

    println("twisted trivalent Delta(5,0)")
    println(`M(5,0)`.toMathemathicaInputString)
  }
}