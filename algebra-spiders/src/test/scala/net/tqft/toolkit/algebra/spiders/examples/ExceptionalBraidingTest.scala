package net.tqft.toolkit.algebra.spiders.examples

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.mathematica.MathematicaForm

@RunWith(classOf[JUnitRunner])
class ExceptionalBraidingTest extends FlatSpec with Matchers with IsomorphismMatchers {

//  "actionOfBraiding on the 3-box space" should "verify" in {
//	  QuantumExceptionalSeries.basisFor3Boxes.verifyActionOfBraiding should be (true)
//  }
  "actionOfBraiding on the 4-box space" should "verify" in {
    val basis = QuantumExceptionalSeries.basisFor4Boxes
    import MathematicaForm._
    println(basis.innerProducts.toMathematicaInputString)
    println(basis.crossingInnerProducts.toMathematicaInputString)
//    println(basis.crossingInnerProducts.toMathematicaInputForm)
	  QuantumExceptionalSeries.basisFor4Boxes.verifyActionOfBraiding should be (true)
  }
//  "actionOfBraiding on the 5-box space" should "verify" in {
//	  QuantumExceptionalSeries.basisFor5Boxes.verifyActionOfBraiding should be (true)
//  }
//  "actionOfBraiding on the 6-box space" should "verify" in {
//	  QuantumExceptionalSeries.basisFor6Boxes.verifyActionOfBraiding should be (true)
//  }

}