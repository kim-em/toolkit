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
    List(0,1,2,3,0,1,2,0,1).splitOn(_ == 0)should equal(List(List(1,2,3), List(1,2), List(1)))
  }
}

