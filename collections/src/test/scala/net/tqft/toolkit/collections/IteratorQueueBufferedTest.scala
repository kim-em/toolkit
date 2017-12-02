package net.tqft.toolkit.collections

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import net.tqft.toolkit.Logging

@RunWith(classOf[JUnitRunner])
class IteratorQueueBufferedTest extends FlatSpec with Matchers {

  "Iterator.queueBuffered" should "work correctly" in {
    import Iterators._

    Iterator.from(0).queueBuffered(1000).take(2000).size should equal(2000)
  }
}

 