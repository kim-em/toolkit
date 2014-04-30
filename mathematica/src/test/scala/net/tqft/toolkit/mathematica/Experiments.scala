package net.tqft.toolkit.mathematica

import com.wolfram.jlink.MathLinkFactory

object Experiments extends App {  
  println(MathematicaKernel.evaluate("GroebnerBasis[{x^2 + x y, y^3 + x y^2}, {y, x}]"))
  
  import Mathematica._
  println("GroebnerBasis[{x^2 + x y, y^3 + x y^2}, {y, x}]".evaluate)
  
  println(MathematicaKernel.evaluate[Expression](Symbols.List(1,2,3)).toInputForm)
  println(Symbols.List(1,2,3).evaluate.toInputForm)
  
  println(Expression("GroebnerBasis[{x^2 + x y, y^3 + x y^2}, {y, x}]").evaluate.toInputForm)
}