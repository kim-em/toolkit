package net.tqft.toolkit.amazon
import net.tqft.toolkit.Logging
import net.tqft.toolkit.collections.Queue
import net.tqft.toolkit.collections.Queues

class SQSQueues(queueService: com.xerox.amazonws.sqs2.QueueService) extends Logging {
  def getQueue(name: String, timeout: Int): Queue[String] = new Queue[String] {
    private[this] val queue = queueService.getOrCreateMessageQueue(name, timeout)

    def enqueue(s: String) = {
      info("Sending message on queue " + name + ": " + s)
      queue.sendMessage(s)
    }
    def dequeue = {
      info("Looking for message on queue " + name)
      val message = queue.receiveMessage()
      if (message == null) {
        None
      } else {
        val s = message.getMessageBody()
        info(". received message: " + s)
        queue.deleteMessage(message)
        Some(s)
      }
    }
  }

  def getPriorityQueue(name: String, timeout: Int, priorityRange: Traversable[Int]) = Queues.priorityQueue({ i: Int => getQueue(name + "-" + i, timeout) }, priorityRange)
}
