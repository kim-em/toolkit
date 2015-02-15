package net.tqft.toolkit.algebra.fusion2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PartialFusionRingTest extends FlatSpec with Matchers {

  "children" should "produce some offspring" in {
	  val children = PartialFusionRingEnumeration(4,0,12.0).root.children
	  for(c <- children) {
	    println(c.entries)
	  }
	  children should be ('nonEmpty)
  }
  "children" should "produce some offspring (2)" in {
	  val children = PartialFusionRingEnumeration(5,0,12.0).root.children
	  for(c <- children) {
	    println(c.entries)
	  }
	  children should be ('nonEmpty)
  }
  "descendants" should "find some the fusion rings" in {
    val enumeration = PartialFusionRingEnumeration(4,0,12.0)
    val descendants = enumeration.root.descendants().toStream
    for(r <- descendants) {
      println(r.matricesToString)
    }
    descendants should be ('nonEmpty)
  }
  "descendants" should "not have any duplicates " in {
    val enumeration = PartialFusionRingEnumeration(4,0,15.0)
    val descendants = enumeration.root.descendants().toStream
    val repeats = descendants.groupBy(_.graphPresentation).mapValues(_.size)
    repeats.values.max should equal (1)
  }
  
}