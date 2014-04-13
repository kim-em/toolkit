package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class ExceptionalBraidingTest extends FlatSpec with Matchers with IsomorphismMatchers {

  "actionOfBraiding on the 3-box space" should "verify" in {
	  QuantumExceptional.verifyActionOfBraiding(QuantumExceptional.basisFor3Boxes)
  }
  "actionOfBraiding on the 4-box space" should "verify" in {
	  QuantumExceptional.verifyActionOfBraiding(QuantumExceptional.basisFor4Boxes)
  }
  "actionOfBraiding on the 5-box space" should "verify" in {
	  QuantumExceptional.verifyActionOfBraiding(QuantumExceptional.basisFor5Boxes)
  }
  "actionOfBraiding on the 6-box space" should "verify" in {
	  QuantumExceptional.verifyActionOfBraiding(QuantumExceptional.basisFor6Boxes)
  }

}