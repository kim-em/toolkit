package net.tqft.toolkit.collections
import scala.collection.SortedMap
import scala.actors.Actor
import net.tqft.toolkit.Throttle
import net.tqft.toolkit.Logging
import scala.actors.IScheduler

trait Queue[A] {
  def enqueue(a: A)
  def enqueue(as: A*) { for (a <- as) enqueue(a) }
  def enqueue(as: Traversable[A]) { for (a <- as) enqueue(a) }
  def dequeue: Option[A]
}

trait PriorityQueue[O, A] extends Queue[(O, A)] { self =>
  def orderBy(f: A => O): Queue[A] = new Queue[A] {
    def enqueue(a: A) = self.enqueue((f(a), a))
    def dequeue = self.dequeue.map(_._2)
  }
}

object Queues {
  implicit def queue2RichQueue[A](q: Queue[A]) = new RichQueue[A](q)
  implicit def priorityQueue2RichPriorityQueue[O, A](q: PriorityQueue[O, A]) = new RichPriorityQueue[O, A](q)

  class RichQueue[A](q: Queue[A]) {
    def toIterable = NonStrictIterable.continually(q.dequeue)

    def consume(f: A => Unit, numberOfWorkers: Int = 1, scheduler: Option[IScheduler] = None): List[Actor] = {
      import Iterables._
      toIterable.flatten.consume(f, numberOfWorkers, scheduler)
    }

    def withDefaultOption(f: => Option[A]): Queue[A] = new Queue[A] {
      def enqueue(a: A) = q.enqueue(a)
      def dequeue = q.dequeue.orElse(f)
    }
    def withDefault(f: => A): Queue[A] = withDefaultOption(Some(f))
    def withDefaultIterable(iterable: Iterable[A]) = {
      import Iterables._
      val f = iterable.asFunction
      withDefaultOption(f())
    }

    def retrying(throttle: Throttle): Queue[A] = new Queue[A] {
      @scala.annotation.tailrec
      def dequeue = {
        val r = q.dequeue
        if (r.isEmpty) {
          throttle(false)
          dequeue
        } else {
          throttle(true)
          r
        }
      }
      def enqueue(a: A) = q.enqueue(a)
    }

    def transform[B](f: A => B, g: B => A): Queue[B] = new TransformedQueue[A, B](q, f, g)

    def passiveBuffering(targetBufferSize: Int): Queue[A] = new Queue[A] {
      val buffer = new ProperlySynchronizedQueue[A]()
      def enqueue(a: A) = q.enqueue(a)
      def dequeue = {
        var r: Option[A] = None
        while (buffer.size < targetBufferSize && { r = q.dequeue; r.nonEmpty }) {
          buffer.enqueue(r.get)
        }
        buffer.dequeueFirst(_ => true)
      }
    }
    def activeBuffering(targetBufferSize: Int): Queue[A] = new Queue[A] {
      val buffer = new ProperlySynchronizedQueue[A]()

      import scala.actors.Actor._
      case object Fill
      val worker = actor {
        loop {
          react {
            case Fill => {
              var r: Option[A] = None
              while (buffer.size < targetBufferSize && { r = q.dequeue; r.nonEmpty }) {
                buffer.enqueue(r.get)
              }
            }
          }
        }
      }
      worker ! Fill

      def enqueue(a: A) = q.enqueue(a)
      def dequeue = {
        val result = buffer.dequeueFirst(_ => true)
        if (buffer.size <= targetBufferSize / 2) worker ! Fill
        result
      }
    }
  }

  class RichPriorityQueue[O, A](q: PriorityQueue[O, A]) {
    def transform[B](f: A => B, g: B => A): PriorityQueue[O, B] = new TransformedQueue[(O, A), (O, B)](q, { p => (p._1, f(p._2)) }, { p => (p._1, g(p._2)) }) with PriorityQueue[O, B]
  }

  private class ProperlySynchronizedQueue[A] extends scala.collection.mutable.SynchronizedQueue[A] {
    override def dequeueFirst(p: A => Boolean): Option[A] = synchronized { super.dequeueFirst(p) }
  }

  private class TransformedQueue[A, B](q: Queue[A], f: A => B, g: B => A) extends Queue[B] {
    def enqueue(b: B) = q.enqueue(g(b))
    def dequeue = q.dequeue.map(f(_))
  }

  def wrap[A](inner: scala.collection.mutable.Queue[A]): Queue[A] = new Queue[A] {
    def enqueue(a: A) = inner.enqueue(a)
    def dequeue = inner.dequeueFirst({ _ => true })
  }

  def create[A]: Queue[A] = wrap(new ProperlySynchronizedQueue[A]())

  def fromOptions[A](f: => Option[A]): Queue[A] = create.withDefaultOption(f)
  def from[A](f: => A): Queue[A] = fromOptions(Some(f))
  def fromIterable[A](iterable: Iterable[A]) = {
    import Iterables._
    val f = iterable.asFunction
    fromOptions(f())
  }

  def priorityQueue[O <% Ordered[O], A](queues: Traversable[(O, Queue[A])]): PriorityQueue[O, A] = new PriorityQueue[O, A] {
    val sortedQueues = scala.collection.SortedMap[O, Queue[A]]() ++ queues
    override def enqueue(p: (O, A)) = sortedQueues(p._1).enqueue(p._2)
    override def dequeue: Option[(O, A)] = {
      var r: Option[A] = None
      sortedQueues.find(p => { r = p._2.dequeue; r.nonEmpty }).map { p => (p._1, r.get) }
    }
  }

  def priorityQueue[O <% Ordered[O], A](queueBuilder: => Queue[A], priorities: Traversable[O]): PriorityQueue[O, A] = priorityQueue({ o: O => queueBuilder }, priorities)

  def priorityQueue[O <% Ordered[O], A](queueBuilder: O => Queue[A], priorities: Traversable[O]): PriorityQueue[O, A] = {
    priorityQueue(priorities.map(o => (o, queueBuilder(o))))
  }

}

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
