package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.mathematica.MathematicaForm

object ComputeInnerProducts extends App {
  {
    val spider = Trivalent.TrivalentSpider

    val `D(4,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(4, 0)
    val `M(4,0)` = spider.innerProductMatrix(`D(4,0)`.toIndexedSeq)

    import net.tqft.toolkit.algebra.mathematica.MathematicaForm._

    println("trivalent Delta(4,0)")
    println(`M(4,0)`.toMathemathicaInputString)    
  }
  {
    val spider = Trivalent.TrivalentSpider

    val `D(5,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(5, 0)
    val `M(5,0)` = spider.innerProductMatrix(`D(5,0)`.toIndexedSeq)

    import net.tqft.toolkit.algebra.mathematica.MathematicaForm._

    println("trivalent Delta(5,0)")
    println(`M(5,0)`.toMathemathicaInputString)    
  }
  {
    val spider = Trivalent.TrivalentSpider

    val `D(6,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(6, 0)
    val `M(6,0)` = spider.innerProductMatrix(`D(6,0)`.toIndexedSeq)

    import net.tqft.toolkit.algebra.mathematica.MathematicaForm._

    println("trivalent Delta(6,0)")
    println(`M(6,0)`.toMathemathicaInputString)    
  }
  
  {
    val spider = TwistedTrivalent.TwistedTrivalentSpider

    val `D(4,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(4, 0)
    val `M(4,0)` = spider.innerProductMatrix(`D(4,0)`.toIndexedSeq)

    import net.tqft.toolkit.algebra.mathematica.MathematicaForm._
    implicit val polynomialForm = MathematicaForm.polynomialMathematicaForm[Fraction[Int]]("w")
    
    println("twisted trivalent Delta(4,0)")
    println(`M(4,0)`.toMathemathicaInputString)    
  }
  {
    val spider = TwistedTrivalent.TwistedTrivalentSpider

    val `D(5,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(5, 0)
    val `M(5,0)` = spider.innerProductMatrix(`D(5,0)`.toIndexedSeq)

    import net.tqft.toolkit.algebra.mathematica.MathematicaForm._
    implicit val polynomialForm = MathematicaForm.polynomialMathematicaForm[Fraction[Int]]("w")
    
    println("twisted trivalent Delta(5,0)")
    println(`M(5,0)`.toMathemathicaInputString)    
  }
}