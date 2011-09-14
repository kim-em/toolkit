package net.tqft.toolkit.amazon
import net.tqft.toolkit.Logging
import net.tqft.toolkit.collections.Queue
import net.tqft.toolkit.collections.Queues
import com.xerox.amazonws.sqs2.MessageQueue
import com.xerox.amazonws.sqs2.SQSException

trait SQSQueue extends Queue[String] {
  def underlyingSQSQueue: MessageQueue
  override def equals(other: Any) = other match {
    case other: SQSQueue => underlyingSQSQueue.getUrl == other.underlyingSQSQueue.getUrl
    case _ => false
  }
}

class SQSQueues(queueService: com.xerox.amazonws.sqs2.QueueService) extends Logging {
  def getQueue(queue: MessageQueue): SQSQueue = new SQSQueue {
    val underlyingSQSQueue = queue
    def enqueue(s: String) = {
      info("Sending message on queue " + queue.getUrl() + ": " + s)
      queue.sendMessage(s)
    }
    def dequeue = {
      info("Looking for message on queue " + queue.getUrl())
      val message = try {
        queue.receiveMessage()
      } catch {
        case e: SQSException => {
          info("tried to read from missing queue " + queue.getUrl())
          null
        }
      }
      if (message == null) {
        None
      } else {
        val s = message.getMessageBody()
        info(". received message: " + s)
        try {
          queue.deleteMessage(message)
        } catch {
          case e: SQSException => info(". something went wrong while trying to delete a message:", e.getMessage())
        }
        Some(s)
      }
    }
  }
  def getQueue(name: String, timeout: Int = 60): Queue[String] = getQueue(queueService.getOrCreateMessageQueue(name, timeout))

  def getPriorityQueue(name: String, timeout: Int = 60, priorityRange: Traversable[Int]) = Queues.priorityQueue({ i: Int => getQueue(name + "-" + i, timeout) }, priorityRange)
}
