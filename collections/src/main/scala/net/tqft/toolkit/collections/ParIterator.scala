package net.tqft.toolkit.collections

import scala.concurrent.{ ExecutionContext, Future, blocking }
import scala.concurrent.Await.result
import scala.concurrent.duration.Duration.Inf
import java.util.concurrent.{ BlockingQueue, Executors, LinkedBlockingQueue }
import scala.concurrent.Await
import scala.concurrent.duration.Duration

// thanks to Juha Heljoranta for this implementation at <http://grokbase.com/t/gg/scala-user/12bx1gp61a/traversing-iterator-elements-in-parallel>
object ParIterator { pi =>

  trait ParIteratorOperations[A] {
    def map[B](f: A => B): Iterator[B]
    def foreach[B](f: A => B)
  }
  
  implicit class ParIterator[A](i: Iterator[A]) {
    def par = new ParIteratorOperations[A] {
      import scala.concurrent.ExecutionContext.Implicits.global
      def map[B](f: A => B): Iterator[B] = pi.map(i)(f)
      def foreach[B](f: A => B) {
        def work: Future[Unit] = {
          val g = Future {
            val o = synchronized {
              if (i.hasNext) {
                Some(i.next)
              } else {
                None
              }
            }
            o.map(a => {
              f(a)
              work
            })
          }
          g.flatMap({
            case None => Future(())
            case Some(a) => a
          })
        }
        Await.result(Future.sequence(for (i <- 0 until Runtime.getRuntime().availableProcessors() + 1) yield work).map(_ => ()), Duration.Inf)
      }
    }
  }

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