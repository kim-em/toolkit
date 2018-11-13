package net.tqft.toolkit.collections

import org.scalatest._

class IteratorTest extends FlatSpec with Matchers {

  "RichIterator" should "work correctly" in {
    import Iterators._

    List(1,3,7,4,-2,5,9).iterator.findMinimum(x => x) should equal (-2)
    
    (0 until 10).iterator.takeEvery(3).toSeq should equal(Seq(0,3,6,9))
  }
}

