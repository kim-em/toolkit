package net.tqft.toolkit.collections

import scala.language.implicitConversions

object Iterators {
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