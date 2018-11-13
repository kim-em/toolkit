package net.tqft.toolkit.algebra.matrices

import org.scalatest._

class FrobeniusPerronEigenvaluesTest extends FlatSpec with Matchers {
	
  "estimate" should "correctly locate the largest eigenvalue of a positive symmetric integer matrix" in {
	  FrobeniusPerronEigenvalues.estimateWithEigenvector(Array(Array(1,2,3), Array(2,3,4), Array(3,4,5)))._1 > 9.6234 should be (true)
	  FrobeniusPerronEigenvalues.estimateWithEigenvector(Array(Array(1,0,0,0), Array(0,25,0,0), Array(0,0,0,0), Array(0,0,0,0)))._1 > 24 should be (true)
   }
}
