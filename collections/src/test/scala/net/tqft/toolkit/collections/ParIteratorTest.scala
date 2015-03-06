package net.tqft.toolkit.collections

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent.duration.Duration
import scala.concurrent.Await

@RunWith(classOf[JUnitRunner])
class ParIteratorTest extends FlatSpec with Matchers {

  "Iterator.par.map" should "work correctly" in {
    import ParIterator._

    Iterator.from(0).par.map(_ + 1).take(10000).toList should equal(1 to 10000)
    (for(x <- Iterator.from(0).par) println(x))
  }
}

 