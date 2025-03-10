package net.tqft.toolkit.collections

import scala.language.implicitConversions
import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.Future
import java.util.concurrent.TimeUnit
import net.tqft.toolkit.Logging
import java.util.concurrent.BlockingQueue
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object Iterators {

  def parallelCombine[A](iterators: Seq[Iterator[A]], queueSize: Int = Integer.MAX_VALUE): Iterator[A] = {
    var running = iterators.size
    val queue = new LinkedBlockingQueue[A]()
    import scala.concurrent.ExecutionContext.Implicits.global
    for (i <- iterators) {
      Future {
        for (a <- i) {
          queue.put(a)
        }
        synchronized {
          running = running - 1
        }
      }
    }
    new Iterator[A] {
      var held: Option[A] = None
      override def hasNext = {
        synchronized {
          held.nonEmpty || {
            while (running > 0 && { held = Option(queue.poll(1, TimeUnit.SECONDS)); held.isEmpty }) {}
            held.nonEmpty || queue.peek != null
          }
        }
      }
      override def next = {
        synchronized {
          if (held.nonEmpty) {
            val r = held.get
            held = None
            r
          } else {
            queue.take
          }
        }
      }
    }
  }

  implicit class QueueBuffered[A](iterator: Iterator[A]) {
    def queueBuffered(bufferSize: Int = 1024): Iterator[A] = queueBuffered(new LinkedBlockingQueue[Option[A]](bufferSize))
    def queueBuffered(queue: BlockingQueue[Option[A]]): Iterator[A] = {
      Future({
        while (iterator.hasNext) {
          queue.put(Some(iterator.next))
        }
        queue.put(None)
      })(ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor()))
      new Iterator[A] {
        var finished = false
        var box: Option[A] = None
        override def hasNext = {
          if (box.nonEmpty) {
            true
          } else {
            if (finished) {
              false
            } else {
              queue.take match {
                case None => {
                  finished = true
                  false
                }
                case Some(a) => {
                  box = Some(a)
                  true
                }
              }
            }
          }
        }
        override def next = {
          box match {
            case Some(a) => {
              val r = a
              box = None
              r
            }
            case None => {
              if (finished) {
                Iterator.empty.next
              } else {
                queue.take.get
              }
            }
          }
        }
      }
    }
  }

  trait PeekableIterator[A] extends Iterator[A] {
    def peek: Option[A]
  }

  implicit class Peekable[A](iterator: Iterator[A]) {
    def peekable: PeekableIterator[A] = {
      iterator match {
        case iterator: PeekableIterator[A] => iterator
        case _ => PeekableIterator.apply(iterator)
      }
    }
  }

  object PeekableIterator {

    def apply[A](iterator: Iterator[A]): PeekableIterator[A] = PeekableIteratorImplementation(iterator)
    private case class PeekableIteratorImplementation[A](iterator: Iterator[A], var nextOption: Option[A] = None) extends PeekableIterator[A] {
      override def hasNext = nextOption.nonEmpty || iterator.hasNext
      override def next = {
        nextOption match {
          case Some(a) => {
            nextOption = None
            a
          }
          case None => iterator.next
        }
      }
      override def peek: Option[A] = {
        nextOption match {
          case Some(a) => Some(a)
          case None => {
            if (iterator.hasNext) {
              nextOption = Some(iterator.next)
              nextOption
            } else {
              None
            }
          }
        }
      }
      override def map[B](f: A => B) = PeekableIteratorImplementation(iterator.map(f), nextOption.map(f))
    }

  }

  implicit class RichIterator[A](iterator: Iterator[A]) {
    def last: A = {
      var a =
        if (iterator.hasNext) {
          iterator.next
        } else {
          throw new NoSuchElementException
        }
      while (iterator.hasNext) {
        a = iterator.next
      }
      a
    }

    def headOption: Option[A] = {
      if (iterator.hasNext) {
        Some(iterator.next)
      } else {
        None
      }
    }

    def takeEvery(k: Int): Iterator[A] = {
      new Iterator[A] {
        override def hasNext = iterator.hasNext
        override def next = {
          val r = iterator.next
          var j = k - 1
          while (iterator.hasNext && j > 0) {
            iterator.next
            j = j - 1
          }
          r
        }
      }
    }

    def distinct: Iterator[A] = {
      new Iterator[A] {
        val store = scala.collection.mutable.Set[A]()
        var nextOption: Option[A] = None
        def hasNext = {
          while (iterator.hasNext && nextOption.isEmpty) {
            val n = iterator.next
            if (!store.contains(n)) {
              store += n
              nextOption = Some(n)
            }
          }
          nextOption.nonEmpty
        }
        def next = {
          val result = nextOption.get
          nextOption = None
          result
        }
      }
    }

    def mapWhileDefined[B](pf: PartialFunction[A, B]) = iterator.takeWhile(pf.isDefinedAt(_)).map(pf)

    //    def consume(f: A => Unit, numberOfWorkers: Int = 1): List[Actor] = {
    //      val si = this.synchronized
    //
    //      class Worker extends Actor { worker =>
    //        var done = false
    //        def act() {
    //          loop {
    //            react {
    //              case 'stop => exit
    //              case 'work if !done =>{
    //                synced.nextOption match {
    //                  case Some(n) => { f(n); worker ! 'work }
    //                  case None => { done = true }
    //                }
    //              }
    //              case 'finished_? if done => reply('done)
    ////                if (si.hasNext) {            
    ////                f(si.next)
    ////                worker ! 'work
    ////              }
    //            }
    //          }
    //        }
    //      }
    //
    //      for (i <- (1 to numberOfWorkers).toList) yield {
    //        val worker = new Worker
    //        worker.start
    //        worker ! 'work
    //        worker
    //      }
    //    }

    object synced {
      def nextOption = synchronized {
        if (iterator.hasNext) Some(iterator.next) else None
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

    def takeWhileThenConsume(p: A => Boolean): Iterator[A] = {
      new Iterator[A] {
        private var box: Option[A] = None
        private def fillBox = {
          if (box.isEmpty && iterator.hasNext) {
            val a = iterator.next
            if (p(a)) {
              box = Some(a)
            } else {
              while (iterator.hasNext) iterator.next
            }
          }
        }
        def hasNext = {
          fillBox
          box.nonEmpty
        }
        def next = {
          val result = box.get
          box = None
          result
        }
      }
    }
  }

  implicit def iterators2RichIterators(iterators: Iterator.type): RichIterator.type = RichIterator
  object RichIterator {
    def iterateUntilNone[A](a: => A)(f: A => Option[A]): Iterator[A] = {
      Iterator.iterate[Option[A]](Some(a))({
        case Some(a) => f(a)
        case None => None
      }).takeWhile({ case Some(_) => true; case None => false }).collect({ case Some(b) => b })
    }
  }

}