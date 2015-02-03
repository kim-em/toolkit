package net.tqft.toolkit.algebra.fusion2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class AssociativityConstraintsTest extends FlatSpec with Matchers {

  "AssociativityConstraints.apply" should "prepare all the associativity and duality equations" in {
    for (e <- AssociativityConstraints(4,0).quadratics) {
      println(e.current)
    }
  }

  "SystemOfQuadratics.preferredSubstitutionVariables" should "report the best variables to evaluate next" in {
    val s = AssociativityConstraints(6, 0)
    println(s.preferredSubstitutionVariables)
    val s2 = s.substitute((1,1,1), 0).get
    println(s2.preferredSubstitutionVariables)
    val s3 = s2.substitute((1,1,2), 0).get
    println(s3.preferredSubstitutionVariables)
  }

}