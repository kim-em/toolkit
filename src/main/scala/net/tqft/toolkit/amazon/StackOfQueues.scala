package net.tqft.toolkit.amazon
import net.tqft.toolkit.collections.Queue
import com.xerox.amazonws.sqs2.MessageQueue
import scala.collection.JavaConversions

object StackOfQueues {
 def apply(prefix: String = "", pushNewQueue: Boolean = false): Queue[String] = {
   val queueService = QueueServiceDefaultAccount()
   if(pushNewQueue) {
     queueService.getOrCreateMessageQueue(prefix + (Long.MaxValue - System.currentTimeMillis).toString)
   }
   
   def createQueuePool = {
     import JavaConversions._
     val availableQueues: List[MessageQueue] = queueService.listMessageQueues(prefix)
     availableQueues
   }
   null
 }
}