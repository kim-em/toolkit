package net.tqft.toolkit.collections

import org.scalatest._
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import net.tqft.toolkit.Logging

class ParIteratorTest extends FlatSpec with Matchers {

  "Iterator.par.map" should "work correctly" in {
    import ParIterator._

    Iterator.from(0).par.map(_ + 1).take(10000).toList should equal(1 to 10000)
    for(x <- Iterator.empty.asInstanceOf[Iterator[Int]].par) println(x)
    (for(x <- Iterator.from(0).take(10000).par) Logging.info(x))
  }
}

 