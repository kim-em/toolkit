package net.tqft.toolkit.collections

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class SubsetsTest extends FlatSpec with Matchers {

  "Subsets" should "work correctly" in {
    import Subsets._
    List("aa", "bcd", "ac", "bbb", "ccc").subsets.toSeq.size should equal(32)
  }

}

