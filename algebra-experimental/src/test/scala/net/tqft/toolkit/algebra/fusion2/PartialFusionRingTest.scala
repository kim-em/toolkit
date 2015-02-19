package net.tqft.toolkit.algebra.fusion2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PartialFusionRingTest extends FlatSpec with Matchers {

//  "children" should "produce some offspring" in {
//    val children = PartialFusionRingEnumeration(4, 0, 12.0).root.children
//    for (c <- children) {
//      println(c.entries)
//    }
//    children should be('nonEmpty)
//  }
//  "children" should "produce some offspring (2)" in {
//    val children = PartialFusionRingEnumeration(5, 0, 12.0).root.children
//    for (c <- children) {
//      println(c.entries)
//    }
//    children should be('nonEmpty)
//  }
//  "descendants" should "not have any duplicates " in {
//    val enumeration = PartialFusionRingEnumeration(4, 0, 15.0)
//    val descendants = enumeration.root.descendants().toStream
//    val repeats = descendants.groupBy(_.graphPresentation).mapValues(_.size)
//    repeats.values.max should equal(1)
//  }
//  "descendants(2, 0, 2.01)" should "find Z/2Z" in {
//    val enumeration = PartialFusionRingEnumeration(2, 0, 2.01)
//    val descendants = enumeration.root.descendants().toStream
//    descendants.filter(_.remaining.isEmpty).size should equal(1)
//  }
//  "descendants(2, 0, 3.62)" should "find T_2" in {
//    val enumeration = PartialFusionRingEnumeration(2, 0, 3.62)
//    val descendants = enumeration.root.descendants().toStream
//    //    for(r <- descendants) println(r)
//    descendants.filter(_.remaining.isEmpty).size should equal(2)
//  }
//  "descendants(3, 0, 3.01)" should "find nothing" in {
//    val enumeration = PartialFusionRingEnumeration(3, 0, 3.01)
//    val descendants = enumeration.root.descendants().toStream
//    //    for (r <- descendants) println(r)
//    descendants.filter(_.remaining.isEmpty).size should equal(0)
//  }
//  "descendants(1, 1, 3.01)" should "find Z/3Z" in {
//    val enumeration = PartialFusionRingEnumeration(1, 1, 3.01)
//    val descendants = enumeration.root.descendants().toStream
//    //    for (r <- descendants) println(r)
//    descendants.filter(_.remaining.isEmpty).size should equal(1)
//  }
//  "descendants(3, 0, 6.01)" should "find 1/2 A_5" in { 
//    val enumeration = PartialFusionRingEnumeration(3, 0, 6.01)
//    val descendants = enumeration.root.descendants().toStream
//    for (r <- descendants) println(r)
//    descendants.filter(_.remaining.isEmpty).size should equal(2)
//  }
  "descendants(3, 0, 13.0)" should "find 1/2 E_6" in { 
    val enumeration = PartialFusionRingEnumeration(3, 0, 13.0)
    val descendants = enumeration.root.descendants().toStream
    for (r <- descendants; if r.remaining.isEmpty) println(r)
    descendants.filter(_.remaining.isEmpty).size should equal(3)
  }


}