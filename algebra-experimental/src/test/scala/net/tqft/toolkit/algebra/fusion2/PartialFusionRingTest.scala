package net.tqft.toolkit.algebra.fusion2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.graphs.Dreadnaut

@RunWith(classOf[JUnitRunner])
class PartialFusionRingTest extends FlatSpec with Matchers {

//  "children" should "produce some offspring" in {
//    val children = PartialFusionRingEnumeration(4, 0).root.children
////    for (c <- children) {
////      println(c.entries)
////    }
//    children should be('nonEmpty)
//  }
//  "children" should "produce some offspring (2)" in {
//    val children = PartialFusionRingEnumeration(5, 0).root.children
////    for (c <- children) {
////      println(c.entries)
////    }
//    children should be('nonEmpty)
//  }
//  "descendants" should "not have any duplicates " in {
//    val enumeration = PartialFusionRingEnumeration(4, 0, Some(0.0))
//    val descendants = enumeration.root.descendants({r => 3-r.steps}).toStream
////     for (r <- descendants) println(Seq.fill(r.steps)(" ").mkString + r.toShortString)
//     val groups = descendants.groupBy(_.graphPresentation)
//     for(group <- groups.values; if group.size > 1) {
//       println(group.size)
//       for(g <- group) {
//         println(g)
//         println(g.toShortString)
//       }
//     }
//    val repeats = descendants.groupBy(r => Dreadnaut.canonicalize(r.graphPresentation)).mapValues(_.size)
//    repeats.values.max should equal(1)
//  }
//  "descendants(2, 0, 2.01)" should "find Z/2Z" in {
//    val enumeration = PartialFusionRingEnumeration(2, 0, Some(2.01))
//    val descendants = enumeration.root.descendants().toStream
//    descendants.filter(_.remaining.isEmpty).size should equal(1)
//  }
//  "descendants(2, 0, 3.62)" should "find T_2" in {
//    val enumeration = PartialFusionRingEnumeration(2, 0, Some(3.62))
//    val descendants = enumeration.root.descendants().toStream
//    //    for(r <- descendants) println(r)
//    descendants.filter(_.remaining.isEmpty).size should equal(2)
//  }
  "descendants(3, 0, 3.01)" should "find nothing" in {
    val enumeration = PartialFusionRingEnumeration(3, 0, Some(3.01))
    val descendants = enumeration.root.descendants().toStream
        for (r <- descendants) println(r)
    descendants.filter(_.remaining.isEmpty).size should equal(0)
  }
//  "descendants(1, 1, 3.01)" should "find Z/3Z" in {
//    val enumeration = PartialFusionRingEnumeration(1, 1, Some(3.01))
//    val descendants = enumeration.root.descendants().toStream
//    //    for (r <- descendants) println(r)
//    descendants.filter(_.remaining.isEmpty).size should equal(1)
//  }
//  "descendants(3, 0, 4.01)" should "find A_3" in { 
//    val enumeration = PartialFusionRingEnumeration(3, 0, Some(4.01))
//    val descendants = enumeration.root.descendants().toStream
////    for (r <- descendants) println(r)
//    descendants.filter(_.remaining.isEmpty).size should equal(1)
//  }
//  "descendants(3, 0, 6.01)" should "find A_3 and 1/2 A_5" in { 
//    val enumeration = PartialFusionRingEnumeration(3, 0, Some(6.01))
//    val descendants = enumeration.root.descendants().toStream
////    for (r <- descendants) println(r)
//    descendants.filter(_.remaining.isEmpty).size should equal(2)
//  }
//  "descendants(3, 0, 9.3)" should "find A_3, 1/2 A_5, and 1/2 A_6" in { 
//    val enumeration = PartialFusionRingEnumeration(3, 0, Some(9.3))
//    val descendants = enumeration.root.descendants().toStream
////    for (r <- descendants) println(r)
//    descendants.filter(_.remaining.isEmpty).size should equal(3)
//  }
//  "descendants(3, 0, 13.0)" should "find A_3, 1/2 A_5, 1/2 A_6, and 1/2 E_6" in { 
//    val enumeration = PartialFusionRingEnumeration(3, 0, Some(13.0))
//    val descendants = enumeration.root.descendants().toStream
//    for (r <- descendants; if r.remaining.isEmpty) println(r)
//    descendants.filter(_.remaining.isEmpty).size should equal(4)
//  }

}