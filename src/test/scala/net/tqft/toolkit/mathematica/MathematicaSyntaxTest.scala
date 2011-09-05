package net.tqft.toolkit.mathematica

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
 
@RunWith(classOf[JUnitRunner])
class MathematicaSyntaxTest extends FlatSpec with ShouldMatchers {

  import MathematicaSyntax._
	
  "A function" should "implicitly implement /@" in {
	  val square = ((n: Int) => n*n)
	  val result: List[Int] = square /@ List(1,2,3)
	  result should equal (List(1, 4, 9))
	  val result2: Set[Int] = { n: Int => n+1 } /@ Set(1,2,3)
	  result2 should equal (Set(2, 3, 4))
  }

	
}