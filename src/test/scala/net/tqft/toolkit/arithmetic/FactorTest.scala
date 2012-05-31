package net.tqft.toolkit.arithmetic

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FactorTest extends FlatSpec with ShouldMatchers {

	"factor" should "factor 60" in {
	  Factor(0) should equal (List(0))
	  Factor(1) should equal (List())
	  Factor(60) should equal (List(2,2,3,5))
	}
	"ecm.factor" should "factor 60" in {
	  ecm.Factor(0) should equal (List(0))
	  ecm.Factor(1) should equal (List())
	  ecm.Factor(60) should equal (List(2,2,3,5))
	}
  
}
 