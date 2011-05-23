package net.tqft.toolkit

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class FancyGroupByTest extends FlatSpec with ShouldMatchers {
  
  "FancyGroupBy" should "work correctly" in {
    import FancyGroupBy._
    List("aa", "bcd", "ac", "bbb", "ccc").fancyGroupBy(_.size, { (x: String, y: String) => (x.size == y.size && x(0) == y(0)) }) should equal (List(List("ac", "aa"), List("ccc"), List("bbb", "bcd")))
  }
  
}

