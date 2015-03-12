package net.tqft.toolkit.algebra.fusion2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class HaagerupRank4Test extends FlatSpec with Matchers {

  val enumeration=PartialFusionRingEnumeration(4, 0)
  val H1 = enumeration.PartialFusionRing("4,0 2 111121110121221111110111010 35.72")
//  val H1_ = enumeration.PartialFusionRing("4,0 1 1111_11101_1__1111110111010 24.29")
//  val H1_p = enumeration.PartialFusionRing("4,0 1 1_1__1110__1__1111110111010 24.29")
//  val r = enumeration.PartialFusionRing("4,0 1 ________0___________0___0_0 24.29")
  
//  println(H1_)
//  
//  for(c <- H1_p.children) {
//    println(c.toShortString)
//    println(c)
//  }
  
//  for(r <- H1_.ancestry) {
//    println(r.toShortString)
//    println(r)
//    println(r.verifyParent)
//  }
  
//  for(r <- H1.isomorphs) {
//    println(r)
//    println(r.verifyAncestry)
//  }
  
//  for(p <- r.descendants(pfr => if(pfr.extendsTo(H1)) 1 else -1)) {
//    println(Seq.fill(p.steps)(" ").mkString + p.toShortString)
//  }
//  
//  
  "the rank 4 Haagerup category" should "have a valid ancestry" in {
    H1.verifyAncestryForSomeIsomorph should be (true)
  }
}