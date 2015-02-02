package net.tqft.toolkit.algebra.fusion2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class AssociativityConstraintsTest extends FlatSpec with Matchers {

  "AssociativityConstraints.apply" should "prepare all the associativity and duality equations" in {
    for(e <- AssociativityConstraints(2, 1).distinct) {
      println(e)
    }
  }

}