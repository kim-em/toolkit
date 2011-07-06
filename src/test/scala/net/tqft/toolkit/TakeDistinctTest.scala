package net.tqft.toolkit

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class TakeDistinctTest extends FlatSpec with ShouldMatchers {
  
  "TakeDistinct" should "work correctly" in {
    import TakeDistinct._
    List(1,2,3,1,5,6).takeDistinct should equal (List(1,2,3))
  }
  
}

