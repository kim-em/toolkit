package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra._
import org.scalatest._
import scala.language.implicitConversions

class AlternatingGroupsTest extends FlatSpec with Matchers {

  val A_4 = FiniteGroups.alternatingGroup(4)
  val A_5 = FiniteGroups.alternatingGroup(5)

  "elements" should "have the right size" in {
    A_4.size should equal(12)
    A_5.size should equal(60)
  }

  "conjugacyClasses" should "have the right size" in {
    A_4.conjugacyClasses.size should equal(4)
  }

  "conjugacyClasses" should "return the class of the identity first" in {
    for (g <- List(A_4, A_5); c = g.conjugacyClasses.head) {
      c.size should equal(1)
      c.representative should equal(g.one)
    }
  }

  "A_5" should "have the right character table" in {
    implicit def lift(k: Int): Polynomial[Fraction[Int]] = Polynomial(0 -> Conversions.integersAsRationals(k))
    A_5.characters should equal(
      Seq[Seq[Polynomial[Fraction[Int]]]](
        Seq(1, 1, 1, 1, 1),
        Seq(3, Polynomial(0 -> 1, 2 -> -1, 3 -> -1, 7 -> 1), Polynomial(2 -> 1, 3 -> 1, 7 -> -1), -1, 0),
        Seq(3, Polynomial(2 -> 1, 3 -> 1, 7 -> -1), Polynomial(0 -> 1, 2 -> -1, 3 -> -1, 7 -> 1), -1, 0),
        Seq(4, -1, -1, 0, 1),
        Seq(5, 0, 0, 1, -1)))
  }

  "A_5" should "have the right tensor product multiplicities" in {
    A_5.tensorProductMultiplicities should equal(List(
      List(List(1, 0, 0, 0, 0), List(0, 1, 0, 0, 0),
        List(0, 0, 1, 0, 0), List(0, 0, 0, 1, 0), List(0, 0, 0, 0, 1)),
      List(List(0, 1, 0, 0, 0), List(1, 1, 0, 0, 1),
        List(0, 0, 0, 1, 1), List(0, 0, 1, 1, 1), List(0, 1, 1, 1, 1)),
      List(List(0, 0, 1, 0, 0), List(0, 0, 0, 1, 1),
        List(1, 0, 1, 0, 1), List(0, 1, 0, 1, 1), List(0, 1, 1, 1, 1)),
      List(List(0, 0, 0, 1, 0), List(0, 0, 1, 1, 1),
        List(0, 1, 0, 1, 1), List(1, 1, 1, 1, 1), List(0, 1, 1, 1, 2)),
      List(List(0, 0, 0, 0, 1), List(0, 1, 1, 1, 1),
        List(0, 1, 1, 1, 1), List(0, 1, 1, 1, 2), List(1, 1, 1, 2, 2))))
  }
  "A_7" should "compute tensor product multiplicities without choking" in {
    FiniteGroups.alternatingGroup(7).tensorProductMultiplicities
  }
}
 

