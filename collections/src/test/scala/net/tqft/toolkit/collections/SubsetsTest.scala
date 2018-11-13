package net.tqft.toolkit.collections

import org.scalatest._

import scala.math._

class SubsetsTest extends FlatSpec with Matchers {

  "Subsets" should "work correctly" in {
    import Subsets._
    List("aa", "bcd", "ac", "bbb", "ccc").subsets.toSeq.size should equal(32)
  }

}

