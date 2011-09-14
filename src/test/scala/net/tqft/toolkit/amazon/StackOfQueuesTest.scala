package net.tqft.toolkit.amazon

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.tqft.toolkit.Logging

@RunWith(classOf[JUnitRunner])
class StackOfQueuesTest extends FlatSpec with ShouldMatchers with Logging {

  val prefix = "net_tqft_toolkit_test"

  Logging.info("Cleaning up...")
  val q = StackOfQueues(prefix)
  while (q.dequeue.nonEmpty) {}
  Logging.info("Finished cleaning up.")
  Thread.sleep(1000)

  "StackOfQueues" should "work correctly" in {
    Logging.info("Running first test ...")
    val stack1 = StackOfQueues(prefix)
    stack1.enqueue("abc")
    Thread.sleep(1000)
    stack1.dequeue should equal(Some("abc"))
    Logging.info("Finished first test.")
  }
  "StackOfQueues" should "work correctly (2)" in {
    Logging.info("Running second test ...")
    val stack1 = StackOfQueues(prefix)
    stack1.enqueue("hello")
    val stack2 = StackOfQueues(prefix)
    stack2.enqueue("world!")
    Thread.sleep(2000)
    List.fill(2)(stack2.dequeue).toSet should equal(Set(Some("hello"), Some("world!")))
    stack1.dequeue should equal(None)
    Logging.info("Finished second test.")
  }

}

