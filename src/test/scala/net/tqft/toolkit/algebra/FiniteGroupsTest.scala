package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FiniteGroupsTest extends FlatSpec with ShouldMatchers {
  
  "dihedralGroup" should "return a group with 2*n elements" in {	  
	  FiniteGroups.dihedralGroup(7).elements.size should equal (14)
  }
  "quotientGroup" should "return a group with the right number of elements" in {
    val g = FiniteGroups.dihedralGroup(4)
    g.verifyInverses
    g.verifyAssociativity
    g.verifyNormalSubgroup(g.elements)
    
    val s1 = FiniteGroups.subgroup(g, Set((0, false), (0, true)))
    val s2 = FiniteGroups.subgroup(g, Set((0, false), (2, false)))
    val s3 = FiniteGroups.subgroup(g, Set((0, false), (2, true)))
    val s4 = FiniteGroups.subgroup(g, Set((0, false), (0, true), (2, false), (2, true)))
    
    for(s <- List(s1,s2,s3,s4)) g.verifySubgroup(s.elements)
    
    FiniteGroups.quotient(g, s1).elements.size should equal(4)
    FiniteGroups.quotient(g, s2).elements.size should equal(4)
    FiniteGroups.quotient(g, s3).elements.size should equal(4)
    FiniteGroups.quotient(g, s4).elements.size should equal(2)
    
    val d6 = FiniteGroups.dihedralGroup(6)
    d6.verifyInverses
    d6.verifyAssociativity
    d6.verifyNormalSubgroup(d6.elements)

    val s = FiniteGroups.subgroup(d6, Set((0,true), (3,true), (0,false), (3,false)))
    s.verifyInverses
    
    FiniteGroups.quotient(d6, s).elements.size should equal(3)
  }
  "doubleCosets" should "work" in {
    val d6 = FiniteGroups.dihedralGroup(6)
    val h = FiniteGroups.subgroup(d6, Set((1,true), (4,true), (0,false), (3,false)))
    d6.verifySubgroup(h.elements)
    val g = FiniteGroups.subgroup(d6, Set((0,true), (3,true), (0,false), (3,false)))
    d6.verifySubgroup(g.elements)
    
    FiniteGroups.doubleCosets(d6, h, g).size should equal (2)
  }
}
