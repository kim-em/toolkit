package net.tqft.toolkit.collections

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class SplitTest extends FlatSpec with ShouldMatchers {

  "splitOn" should "work correctly" in {
    import Split._
    List(0, 1, 2, 3, 0, 1, 2, 0, 1).splitOn(_ == 0) should equal(List(List(1, 2, 3), List(1, 2), List(1)))
  }

  "splitAfter" should "work" in {
    import Split._
    List(2, 3, 3, 4, 4, 5, 7, 9, 2, 4, 1, 6).iterator.splitAfter(_ % 2 == 0).toList should equal(List(List(2), List(3, 3, 4), List(4), List(5, 7, 9, 2), List(4), List(1, 6)))
  }

  "splitBefore" should "work" in {
    import Split._
    List(2, 3, 3, 4, 4, 5, 7, 9, 2, 4, 1, 6).iterator.splitBefore(_ % 2 == 0).toList should equal(List(List(), List(2, 3, 3), List(4), List(4, 5, 7, 9), List(2), List(4,1), List(6)))
  }
}

