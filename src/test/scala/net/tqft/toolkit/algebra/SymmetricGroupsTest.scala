package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SymmetricGroupsTest extends FlatSpec with ShouldMatchers {

  val S_4 = FiniteGroups.permutationGroup(4)
  val S_5 = FiniteGroups.permutationGroup(5)

    "S_5" should "have the right class coefficients" in {
	  println(S_4.classCoefficients)
  }

  
  "S_5" should "have the right simultaneous eigenvectors" in {
	  val eigenvectors = S_4.classCoefficientSimultaneousEigenvectorsModPrime
	  println(eigenvectors)
//	  S_5.characterTableModPreferredPrime should equal(120)
  }

}
