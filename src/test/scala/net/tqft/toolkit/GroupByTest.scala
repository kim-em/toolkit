package net.tqft.toolkit

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class GroupByTest extends FlatSpec with ShouldMatchers {

  "GroupBy" should "work correctly" in {
    import GroupBy._
    List("aa", "bcd", "ac", "bbb", "ccc").groupByEquivalence(
      equivalence = { x => x.size },
      invariant = { (x: String, y: String) => (x.size == y.size && x(0) == y(0)) }) should equal(List(List("ac", "aa"), List("ccc"), List("bbb", "bcd")))
  }

}

