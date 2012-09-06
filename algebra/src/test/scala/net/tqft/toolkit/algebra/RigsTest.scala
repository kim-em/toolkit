package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RigsTest extends FlatSpec with ShouldMatchers {
  
  "adjoinUnknown" should "give something sensible" in {	  
	 val N_? = Rigs.adjoinUnknown(Gadgets.Integers)
	 import net.tqft.toolkit.UnionTypes._
	 import Unknowns.?
	 N_?.multiply(2, 3) should equal(Left(6))
	 N_?.multiply(2, ?) should equal(Right(?))
	 N_?.multiply(?, ?) should equal(Right(?))
	 N_?.multiply(0, ?) should equal(Left(0))
	 N_?.multiply(?, 0) should equal(Left(0))
	 N_?.add(2, 3) should equal(Left(5))
	 N_?.add(2, ?) should equal(Right(?))
	 N_?.add(?, ?) should equal(Right(?))
	 N_?.add(0, ?) should equal(Right(?))
	 N_?.add(?, 0) should equal(Right(?))
  }
}
