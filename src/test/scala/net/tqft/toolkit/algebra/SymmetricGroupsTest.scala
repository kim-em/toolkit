package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SymmetricGroupsTest extends FlatSpec with ShouldMatchers {

  val A_4 = FiniteGroups.alternatingGroup(4)
  val S_4 = FiniteGroups.symmetricGroup(4)
  val S_5 = FiniteGroups.symmetricGroup(5)
  
  "elements" should "have the right size" in {
    A_4.size should equal(12)
    S_4.size should equal(24)
    S_5.size should equal(120)
  }
  
  "conjugacyClasses" should "have the right size" in {
    A_4.conjugacyClasses.size should equal(4)
  }
  
  "conjugacyClasses" should "return the class of the identity first" in {
    for(g <- List(A_4, S_4, S_5); c = g.conjugacyClasses.head) {
      c.size should equal(1)
      c.representative should equal(g.one)
    }
  }

    "S_5" should "have the right class coefficients" in {
//	  println(S_5.classCoefficients)
  }

  
  "S_5" should "have the right simultaneous eigenvectors" in {    
	  val eigenvectors = A_4.classCoefficientSimultaneousEigenvectorsModPrime
	  println("simultaneous eigenvectors: " + eigenvectors)
	  println(A_4.characterTableModPreferredPrime)
	  println(FiniteGroups.symmetricGroup(7).characterTable)
  }
 
}
 