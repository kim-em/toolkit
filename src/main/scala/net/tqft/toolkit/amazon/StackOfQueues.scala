package net.tqft.toolkit.amazon
import net.tqft.toolkit.collections.Queue
import com.xerox.amazonws.sqs2.MessageQueue
import scala.collection.JavaConversions
import net.tqft.toolkit.collections.Queues
import net.tqft.toolkit.Logging
import com.xerox.amazonws.sqs2.SQSException

object StackOfQueues extends Logging {
  import JavaConversions._
  val queueMaps: scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, SQSQueue]] = new com.google.common.collect.MapMaker().makeMap[String, scala.collection.mutable.Map[String, SQSQueue]]()

  def apply(prefix: String = ""): Queue[String] = {
    val queueService = QueueServiceDefaultAccount()
    val queues = new SQSQueues(queueService)

    if(queueMaps.get(prefix).isEmpty) {
      queueMaps += prefix -> new com.google.common.collect.MapMaker().makeMap[String, SQSQueue]()
    }
    
    val queuePool = scala.collection.mutable.Map[Int, Queue[String]]()

    def updateQueuePool {
      info("updating the queue pool")
      val newValues = JavaConversions.asScalaBuffer(queueService.listMessageQueues(prefix)).map(q => q.getUrl.toString -> queues.getQueue(q)).toList
      queueMaps(prefix) ++= newValues.filterNot { p: (String, Queue[String]) => queueMaps(prefix).keySet.contains(p._1) }
      regenerateQueuePool
    }
    def offerQueues(newQueues: Traversable[MessageQueue]) {
      queueMaps(prefix) ++= newQueues.map(q => q.getUrl.toString -> queues.getQueue(q))
      regenerateQueuePool
    }
    def regenerateQueuePool {
      def wrap(p: (Int, SQSQueue)) = {
        val (k, q) = p
        def deleteQueue(dq: MessageQueue)() = {
          info("Attempting to delete queue " + dq.getUrl)
          try {
            dq.deleteQueue()
          } catch {
            case e: SQSException => info("It looks like this queue has already been deleted: " + dq.getUrl)
          }
          queueMaps -= dq.getUrl.toString
        }

        import Queues._
        
        val nq = if(k == 0) q else  q.notifyAfterFailures(1, deleteQueue(q.underlyingSQSQueue) _)
        k -> nq
      }

      queuePool.clear
      queuePool ++= queueMaps(prefix).toList.sortBy(_._1).map(_._2).zipWithIndex.map(_.swap).map(wrap)
    }

    def createNewPool = {
      val name = prefix + (Long.MaxValue - System.currentTimeMillis).toString
      info("Creating new queue " + name)
      val newQueue = queueService.getOrCreateMessageQueue(name)
      offerQueues(Some(newQueue))
      regenerateQueuePool
    }

    import Queues._
    val queue = Queues.priorityQueue(queuePool).orderBy({ _ => 0 })
    queue.triggerOnFirstEnqueue(createNewPool _).triggerOnFirstDequeue(updateQueuePool _).notifyAfterFailures(5, updateQueuePool _)
  }
}