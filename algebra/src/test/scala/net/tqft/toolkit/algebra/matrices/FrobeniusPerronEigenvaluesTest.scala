package net.tqft.toolkit.algebra.matrices

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

@RunWith(classOf[JUnitRunner])
class FrobeniusPerronEigenvaluesTest extends FlatSpec with ShouldMatchers {
	
  "estimate" should "correctly locate the largest eigenvalue of a positive symmetric integer matrix" in {
	  FrobeniusPerronEigenvalues.estimate(List(List(1,2,3), List(2,3,4), List(3,4,5))) > 9.6234 should be (true)
	  FrobeniusPerronEigenvalues.estimate(List(List(1,0,0,0), List(0,25,0,0), List(0,0,0,0), List(0,0,0,0))) > 24 should be (true)
   }
}
