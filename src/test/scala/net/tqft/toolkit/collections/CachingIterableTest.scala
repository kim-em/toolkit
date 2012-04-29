package net.tqft.toolkit.collections

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CachingIterableTest extends FlatSpec with ShouldMatchers {

  val counting = NonStrictIterable.iterate(0)({ i => { Thread.sleep(10); i + 1 } })

  "CachingIterable" should "survive concurrent use" in {
    import Iterables._
    val i = CachingIterable(0 until 100)
    i.consume(n => n, 16)
    Thread.sleep(500)
    i.toList should equal((0 until 100).toList)
  }

}