package net.tqft.toolkit.collections

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.amazon.QueueServiceDefaultAccount
import net.tqft.toolkit.Throttle
import net.tqft.toolkit.amazon.SQSQueues

@RunWith(classOf[JUnitRunner])
class SQSQueuesTest extends FlatSpec with ShouldMatchers {

  "SQSQueues" should "work correctly" in {
    import Queues._

    val SQS = new SQSQueues(QueueServiceDefaultAccount())

    val p = SQS.getPriorityQueue("net-tqft-toolkit-testing", 60, 0 to 1).transform[Int](_.toInt, _.toString).orderBy(_ % 2).retrying(Throttle.linearBackoff(1000))

    p.enqueue(List(3, 2, 1, 0))
    // firing off so few messages so fast, it's unreasonable to expect prioritization to work
    p.toIterable.take(4).toSet should equal(Set(Some(2), Some(0), Some(3), Some(1)))
  }
}

