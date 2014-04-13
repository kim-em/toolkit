package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class ExceptionalBraidingTest extends FlatSpec with Matchers with IsomorphismMatchers {

  "actionOfBraiding on the 3-box space" should "verify" in {
	  QuantumExceptional.basisFor3Boxes.verifyActionOfBraiding
  }
  "actionOfBraiding on the 4-box space" should "verify" in {
	  QuantumExceptional.basisFor4Boxes.verifyActionOfBraiding
  }
  "actionOfBraiding on the 5-box space" should "verify" in {
	  QuantumExceptional.basisFor5Boxes.verifyActionOfBraiding
  }
  "actionOfBraiding on the 6-box space" should "verify" in {
	  QuantumExceptional.basisFor6Boxes.verifyActionOfBraiding
  }

}