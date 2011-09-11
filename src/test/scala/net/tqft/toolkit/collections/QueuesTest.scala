package net.tqft.toolkit.collections

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.Throttle

@RunWith(classOf[JUnitRunner])
class QueuesTest extends FlatSpec with ShouldMatchers {

  "Queues" should "work correctly" in {
    import Queues._

    val q = Queues.create[String]
    q.enqueue("a", "b")
    q.toIterable.take(3).toList should equal(List(Some("a"), Some("b"), None))

    val random = new scala.util.Random()
    def f = if (random.nextBoolean) { Some(1) } else { None }

    Queues.fromOptions(f).retrying(Throttle.none).toIterable.take(10).toList should equal(List.fill(10)(Some(1)))
  }

  "Queues" should "work correctly (2)" in {
    import Queues._

    val p = Queues.priorityQueue(Queues.create[Int], List(0, 1)).orderBy(_ % 2)

    p.enqueue(3, 2, 1, 0)
    p.toIterable.take(5).toList should equal(List(Some(2), Some(0), Some(3), Some(1), None))
  }

  "consume" should "consume a queue via a pool of actors" in {
    case class Stop
    val p = Queues.create[Either[String, Stop]]
    val words = List("foo", "bar", "baz", "turkle", "qux")

    p enqueue(words.map(Left(_)))
    p enqueue(Right(Stop()))
    val q = Queues.create[String]

    val threads = new scala.collection.mutable.HashSet[String] with scala.collection.mutable.SynchronizedSet[String]

    import Queues._
    import Iterables._
    p.toIterable.flatten.takeWhile(_.isLeft).collect{case Left(s) => s}.consume({ s => { threads += Thread.currentThread.getName;  q.enqueue(s) } }, 2)
    q.retrying(Throttle.rateLimited(50)).toIterable.flatten.take(5).toSet should equal(words.toSet)
  }

  "activeBuffering" should "actively buffer!" in {
    var counter = 0
    import Queues._
    val q = Queues.from({ counter = counter + 1; 0 }).activeBuffering(10)
    Thread.sleep(100)
    counter should equal(10)
    for(_ <- 1 to 5) q.dequeue
    Thread.sleep(100)
    counter should equal(15)
  }
}

