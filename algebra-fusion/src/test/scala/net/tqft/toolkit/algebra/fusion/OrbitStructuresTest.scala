package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups

@RunWith(classOf[JUnitRunner])
class OrbitStructuresTest extends FlatSpec with Matchers {

  "OrbitStructures" should "return all the ways to build orbit structures for a given global dimension" in {
    
    for(os <- OrbitStructures(36)) {
      println(os)
    }
  }
}