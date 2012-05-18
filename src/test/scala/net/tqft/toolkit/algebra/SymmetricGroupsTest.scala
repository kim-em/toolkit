package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SymmetricGroupsTest extends FlatSpec with ShouldMatchers {

  val S_3 = FiniteGroups.symmetricGroup(3)
  val S_4 = FiniteGroups.symmetricGroup(4)
  val S_5 = FiniteGroups.symmetricGroup(5)

  "elements" should "have the right size" in {  
    S_4.size should equal(24)
    S_5.size should equal(120)
  }

  "conjugacyClasses" should "return the class of the identity first" in {
    for (g <- List(S_4, S_5); c = g.conjugacyClasses.head) {
      c.size should equal(1)
      c.representative should equal(g.one)
    }
  }

  "S_3" should "have the right tensor product multiplicities" in {
     S_3.tensorProductMultiplicities should equal (List(List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1)), List(List(0, 1, 0), List(1, 0, 0), List(0, 0, 1)), List(List(0, 0, 1), List(0, 0, 1), List(1, 1, 1))))
  }
  
  "S_5" should "have the right character table" in {
    import Implicits.{ Rationals, integersAsRationals, asConstants }
    implicit def lift(k: Int): Polynomial[Fraction[Int]] = asConstants(integersAsRationals(k))
    
    S_5.characterTable._2 should equal(
      Seq[Seq[Polynomial[Fraction[Int]]]](
        Seq(1, 1, 1, 1, 1, 1, 1),
        Seq(1, -1, 1, 1, -1, 1, -1),
        Seq(4, 2, 0, 1, -1, -1, 0),
        Seq(4, -2, 0, 1, 1, -1, 0),
        Seq(5, 1, 1, -1, 1, 0, -1),
        Seq(5, -1, 1, -1, -1, 0, 1),
        Seq(6, 0, -2, 0, 0, 1, 0)
        ))
  }
}
