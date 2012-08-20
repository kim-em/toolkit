package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FrobeniusPerronEigenvaluesTest extends FlatSpec with ShouldMatchers {

  "estimate" should "correctly locate the largest eigenvalue of a positive symmetric integer matrix" in {
	  FrobeniusPerronEigenvalues.estimate(List(List(1,2,3), List(2,3,4), List(3,4,5))) > 9.6234 should be (true)
   }
}
