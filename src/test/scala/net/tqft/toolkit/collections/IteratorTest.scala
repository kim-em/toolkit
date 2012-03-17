package net.tqft.toolkit.collections

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.Throttle

@RunWith(classOf[JUnitRunner])
class IteratorTest extends FlatSpec with ShouldMatchers {

  "RichIterator" should "work correctly" in {
    import Iterators._

    List(1,3,7,4,-2,5,9).iterator.findMinimum(x => x) should equal (-2)
  }
}

