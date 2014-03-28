package net.tqft.toolkit.collections

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class IterablesTest extends FlatSpec with Matchers {
  
  "findMinimum" should "work correctly" in {
    import Iterables._
    List(5,4,3,2,1).findMinimum(_ % 2, lowerBound = Some(0)) should equal (4) 
    List(4,3,2,1).findMinimum(_ % 2, lowerBound = Some(0)) should equal (4) 
  }
  
}

