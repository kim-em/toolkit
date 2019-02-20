package net.tqft.toolkit.algebra.lattices

import org.scalatest._
import scala.math.BigInt.int2bigInt

class LLLTest extends FlatSpec with Matchers {

  "LLL" should "should do an example correctly" in {
	  LLL(List(List(1,-1,3),List(1,0,5),List(1,2,6))) should equal (List(List(0,1,-1), List(1,0,0),List(0,1,2)): Seq[Seq[BigInt]])
  }
}
