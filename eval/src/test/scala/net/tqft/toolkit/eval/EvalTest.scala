package net.tqft.toolkit.eval

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class EvalTest extends FlatSpec with ShouldMatchers {
  
  "Eval" should "work correctly" in {
	  Eval("def plusOne(x: Int) = x + 1")
	  Eval("plusOne(5)") should equal(Some(6))
  }
  
}

