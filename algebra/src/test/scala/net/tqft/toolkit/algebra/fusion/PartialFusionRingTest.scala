package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.algebra.matrices.Matrices
import net.tqft.toolkit.algebra.grouptheory.FiniteGroup

@RunWith(classOf[JUnitRunner])
class PartialFusionRingTest extends FlatSpec with ShouldMatchers {

  import net.tqft.toolkit.functions.Memo._
  implicit val toPartialFusionRing = { group: FiniteGroup[_] =>
    FusionRings.Examples.representationsOf(group): PartialFusionRing
  }.memo

  //    "verifyAncestry" should "work on cyclic groups" in {
  //      for (n <- 4 to 7) {
  //        val pfr: PartialFusionRing = FiniteGroups.cyclicGroup(n)
  //        pfr.verifyAncestry should be(true)
  //      }
  //    }

//  val pfr: PartialFusionRing = FiniteGroups.symmetricGroup(4)
//  println(pfr)
//  println(pfr.parent.get)
//  val p2 = pfr.parent.get.parent.get
//  println("p2 -> " + p2)
//  val p3 = pfr.parent.get.parent.get.parent.get
//  println("p3.children.exists(_.isomorphicTo_?(p2)) -> " + p3.children.exists(_.isomorphicTo_?(p2)))
//  println("p3 -> " + p3)
//
//  val upperObjects = p3.upperObjects
//  for (o <- upperObjects.orbits.iterator; s <- o.elements.subsets(2); Seq(a, b) = s.toSeq) {
//    require(a.result.isomorphicTo_?(b.result), a + " not isomorphic to " + b)
//  }
//
//  for (s <- upperObjects.orbits.subsets(2); Seq(o1, o2) = s.toSeq; u1 = o1.representative; u2 = o2.representative) {
//    require(!u1.result.isomorphicTo_?(u2.result), "orbits " + o1 + " and " + o2 + " contain isomorphic representatives " + u1 + " and " + u2)
//  }
//
//  require(p3.verifyUpperOrbits.forall(_ == true))
//  val orbit = p3.upperObjects.orbits.find(_.elements.exists(_.result.isomorphicTo_?(p2))).get
//  println("orbit.elements -> " + orbit.elements)
//  println("orbit.elements.head.result.parent -> " + orbit.elements.head.result.parent)
//  println("p3.upperObjects.elements.map(_.result).exists(_.isomorphicTo_?(p2)) -> " + p3.upperObjects.elements.map(_.result).exists(_.isomorphicTo_?(p2)))

          "verifyAncestry" should "work on symmetric groups" in {
            for (n <- 4 to 4) {
              FiniteGroups.symmetricGroup(n).verifyAncestry/*ForSomeIsomorph*/ should be(true)
            }
          }

  //        "progenitor" should "always be A1" in {
  //          for (n <- 4 to 7) {
  //             FiniteGroups.cyclicGroup(n).progenitor.ring.rank should be(1)
  //          }
  //        }

}
