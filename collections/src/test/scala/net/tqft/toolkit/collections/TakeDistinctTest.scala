package net.tqft.toolkit.collections

import org.scalatest._

class TakeDistinctTest extends FlatSpec with Matchers {
  
  "TakeDistinct" should "work correctly" in {
    import TakeDistinct._
    List(1,2,3,1,5,6).takeDistinct should equal (List(1,2,3))
  }
  
}

