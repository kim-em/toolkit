package net.tqft.toolkit.collections

import scala.concurrent.{ ExecutionContext, Future, blocking }
import scala.concurrent.Await.result
import scala.concurrent.duration.Duration.Inf
import java.util.concurrent.{ BlockingQueue, Executors, LinkedBlockingQueue }
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import net.tqft.toolkit.Logging
import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

object ParIterator { pi =>

  trait ParIteratorOperations[A] {
    def map[B](f: A => B): Iterator[B]
    def flatMap[B, That](f: (A) ⇒ GenTraversableOnce[B]): Iterator[B]
    def foreach[B](f: A => B)
  }

  implicit class ParIterator[A](i: Iterator[A]) {
    def par = parWithNumberOfThreads(4 * _ + 1)
    def parWithNumberOfThreads(threadsFromCPUs: Int => Int): ParIteratorOperations[A] = parWithNumberOfThreads(threadsFromCPUs(Runtime.getRuntime().availableProcessors()))
    def parWithNumberOfThreads(threads: Int): ParIteratorOperations[A] = new ParIteratorOperations[A] {

      implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threads))
      def map[B](f: A => B): Iterator[B] = pi.map(i)(f)
      def flatMap[B, That](f: (A) ⇒ GenTraversableOnce[B]): Iterator[B] = {
        pi.map(i)(f).flatMap(x => x)
      }
      def foreach[B](f: A => B) {
        val queue: BlockingQueue[Option[A]] = new LinkedBlockingQueue(threads)

        def work: Future[Unit] = {
          val g = Future {
            queue.take.map(a => {
              f(a)
              work
            })
          }
          g.flatMap({
            case None => {
              //              Logging.info("closing this thread...") 
              Future.successful(())
            }
            case Some(a) => a
          })
        }

        val together = Future.sequence(for (i <- 0 until threads) yield work).map(_ => ())

        i.foreach(x => queue.put(Some(x)))
        
        //          Logging.info("iterator exhausted")
        for (i <- 0 until 2 * threads) {
          queue.put(None)
        }

        Await.result(together, Duration.Inf)
        //        Logging.info("finished foreach")
      }
    }
  }

  // thanks to Juha Heljoranta for this implementation at <http://grokbase.com/t/gg/scala-user/12bx1gp61a/traversing-iterator-elements-in-parallel>
  def map[A, B](i: Iterator[A])(f: A => B)(implicit execctx: ExecutionContext): Iterator[B] = {
    val cpus = Runtime.getRuntime().availableProcessors() + 1
    val queue: BlockingQueue[Option[Future[B]]] = new LinkedBlockingQueue(cpus * cpus)
    Future {
      try {
        blocking {
          i.foreach(x => queue.put(Some(Future { f(x) })))
        }
      } finally {
        queue.put(None) // poison
      }
    }
    new Iterator[B] {

      private[this] var fopt: Option[Future[B]] = None
      private[this] var alive = true

      override def next() =
        if (hasNext) { val v = result(fopt.get, Inf); fopt = None; v }
        else Iterator.empty.next()

      override def hasNext: Boolean = alive && take().isDefined

      private def take(): Option[Future[B]] = {
        if (fopt.isEmpty) {
          fopt = queue.take() match {
            case None => { alive = false; None }
            case some => some
          }
        }
        fopt
      }

    }
  }

}