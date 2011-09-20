package net.tqft.toolkit.collections
import scala.actors.Actor
import scala.actors.IScheduler

object Iterators {
  implicit def iterator2RichIterator[A](iterator: Iterator[A]) = new RichIterator(iterator)

  class RichIterator[A](iterator: Iterator[A]) {
    def mapWhileDefined[B](pf: PartialFunction[A, B]) = iterator.takeWhile(pf.isDefinedAt(_)).map(pf)

    def consume(f: A => Unit, numberOfWorkers: Int = 1, scheduler: Option[IScheduler] = None): List[Actor] = {
      val withScheduler = scheduler.getOrElse(scala.actors.Scheduler)
      val si = this.synchronized

      class Worker extends Actor { worker =>
        override def scheduler = withScheduler
        def act() {
          loop {
            react {
              case 'stop => exit
              case 'work => if (si.hasNext) {
                f(si.next)
                worker ! 'work
              }
            }
          }
        }
      }

      for (i <- (1 to numberOfWorkers).toList) yield {
        val worker = new Worker
        worker.start
        worker ! 'work
        worker
      }
    }

    def synchronized: Iterator[A] = {
      new Iterator[A] {
        def hasNext = synchronized { iterator.hasNext }
        def next = synchronized { iterator.next }
      }
    }

    def asFunction: (() => Option[A]) = new (() => Option[A]) {
      def apply = if (iterator.hasNext) Some(iterator.next) else None
    }

    def findMinimum[B <% Ordered[B]](f: A => B, lowerBound: Option[B] = None): A = {
      if (iterator.hasNext) {
        var next = iterator.next
        var v = f(next)
        if (lowerBound == Some(v)) return next
        var minimum = (next, v)
        while (iterator.hasNext) {
          next = iterator.next
          v = f(next)
          if (lowerBound == Some(v)) return next
          if (v < minimum._2) {
            minimum = (next, v)
          }
        }
        minimum._1
      } else {
        throw new NoSuchElementException
      }

    }
  }
}