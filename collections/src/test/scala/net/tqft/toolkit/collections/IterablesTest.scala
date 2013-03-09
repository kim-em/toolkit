package net.tqft.toolkit.collections

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class IterablesTest extends FlatSpec with ShouldMatchers {
  
  "findMinimum" should "work correctly" in {
    import Iterables._
    List(5,4,3,2,1).findMinimum(_ % 2, lowerBound = Some(0)) should equal (4) 
    List(4,3,2,1).findMinimum(_ % 2, lowerBound = Some(0)) should equal (4) 
  }
  
}

