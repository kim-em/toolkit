package net.tqft.toolkit.algebra.fusion2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class AssociativityConstraintsTest extends FlatSpec with Matchers {

  "PartialFusionRingEnumeration.root" should "prepare all the associativity and duality equations" in {
    val quadratics = PartialFusionRingEnumeration(4, 0).root.associativity.quadratics
    for (e <- quadratics) {
      println(e)
    }
    println(quadratics.size + " quadratics, with " + quadratics.distinct.size + " distinct values.")
  }

  "SystemOfQuadratics.preferredSubstitutionVariables" should "report the best variables to evaluate next" in {
    val s = PartialFusionRingEnumeration(6, 0).root.associativity
    println(s.preferredSubstitutionVariables)
    val s2 = s.substitute((1, 1, 1), 0).get
    println(s2.preferredSubstitutionVariables)
    val s3 = s2.substitute((1, 1, 2), 0).get
    println(s3.preferredSubstitutionVariables)
  }

}