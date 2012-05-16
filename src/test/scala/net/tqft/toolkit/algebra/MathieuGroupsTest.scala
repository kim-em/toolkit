package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MathieuGroupsTest extends FlatSpec with ShouldMatchers {

  val M_12 = FiniteGroups.Mathieu12

  "M_12" should "be a group with 95040 elements" in {
    M_12.size should equal(95040)
  }
  "M_12" should "have 15 conjugacy classes" in {
    M_12.conjugacyClasses.size should equal(15)
  }
  "M_12" should "have the right exponent" in {
    M_12.exponent should equal(1320)
  }
  //  "M_12" should "have the right class coefficients" in {
  //	  println(M_12.classCoefficients)
  //  }
  "M_12" should "have repeated eigenvalues for each class coefficient matrix" in {
	  val eigenvectors = M_12.classCoefficientSimultaneousEigenvectorsModPrime
	  println(eigenvectors)
  }

}
