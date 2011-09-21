package net.tqft.toolkit.collections

import scala.collection.mutable.ListBuffer
import scala.collection.GenIterable
import scala.collection.parallel.ParIterable
import scala.collection.GenSeq
import scala.collection.parallel.ParSeq
object GroupBy {

  implicit def groupable[A](x: GenIterable[A]) = new Groupable(x)
  class Groupable[A](x: GenIterable[A]) {

    def equivalenceClasses(equivalence: (A, A) => Boolean): List[List[A]] = equivalenceClasses[Unit](equivalence, { x => () })

    def equivalenceClasses[B <% Ordered[B]](equivalence: (A, A) => Boolean, invariant: A => B): List[List[A]] = {
      def _equivalenceClasses(y: List[A]) = {
        def acc(classes: ParSeq[List[A]])(z: List[A]): List[List[A]] = z match {
          case Nil => classes.toList
          case a :: r => classes.indexWhere(c => equivalence(c.head, a)) match {
            case -1 => acc(List(a) +: classes)(r)
            case k => acc(classes.updated(k, a :: classes(k)))(r)
          }
        }

        acc(Nil.par)(y)
      }

      import Split._
      ((x.toList sortBy { invariant } splitBy { invariant }).par.map { _equivalenceClasses }).toList.flatten
    }

    def chooseEquivalenceClassRepresentatives(equivalence: (A, A) => Boolean): List[A] = chooseEquivalenceClassRepresentatives[Unit](equivalence, { x => () })
    def chooseEquivalenceClassRepresentatives[B <% Ordered[B]](equivalence: (A, A) => Boolean, invariant: A => B): List[A] = {
      def _chooseEquivalenceClassRepresentatives(y: List[A]) = {
        def acc(representatives: ParSeq[A])(z: List[A]): List[A] = z match {
          case Nil => representatives.toList
          case a :: r => representatives.indexWhere(c => equivalence(c, a)) match {
            case -1 => acc(a +: representatives)(r)
            case k => acc(representatives)(r)
          }
        }

        acc(Nil.par)(y)
      }

      import Split._
      ((x.toList sortBy { invariant } splitBy { invariant }).par.map { _chooseEquivalenceClassRepresentatives }).toList.flatten
    }

    def lazilyGroupBy[B](f: A => B): Iterable[(B, Iterator[A])] = {
      import scala.collection.mutable.Queue
      import scala.collection.mutable.Map

      new Iterable[(B, Iterator[A])] {
        def iterator = new Iterator[(B, Iterator[A])] {
          val underlying = x.iterator

          def processOne {
            val a = underlying.next
            val b = f(a)
            allQueues.get(b) match {
              case None => {
                val newQueue = Queue(a)
                allQueues.put(b, newQueue)
                nextQueues.enqueue((b, newQueue))
              }
              case Some(bQueue) => {
                bQueue.enqueue(a)
              }
            }
          }

          class QueueWrapper(q: Queue[A]) extends Iterator[A] {
            def hasNext = {
              if (q.nonEmpty) {
                true
              } else {
                while (underlying.hasNext && q.isEmpty) {
                  processOne
                }
                q.nonEmpty
              }
            }
            def next = q.dequeue
          }

          val allQueues = Map[B, Queue[A]]()
          val nextQueues = Queue[(B, Queue[A])]()

          def hasNext = {
            if (nextQueues.nonEmpty) {
              true
            } else {
              while (underlying.hasNext && nextQueues.isEmpty) {
                processOne
              }
              nextQueues.nonEmpty
            }
          }

          def next = nextQueues.dequeue match { case (b, q) => (b, new QueueWrapper(q)) }
        }
      }
    }

    def lazilyChooseRepresentatives[B](equivalence: (A, A) => Boolean, invariant: A => B) = new Iterable[A] {
      def iterator = new Iterator[A] {
        import scala.collection.mutable.Map

        val underlying = x.iterator
        val previous = Map[B, ListBuffer[A]]()

        var nextResult: Option[A] = None

        def processOne {
          val a = underlying.next
          val b = invariant(a)
          previous.get(b) match {
            case None => {
              previous.put(b, new ListBuffer() += a)
              nextResult = Some(a)
            }
            case Some(as) => {
              as.find(_a => equivalence(a, _a)) match {
                case Some(_) => ()
                case None => {
                  as += a
                  nextResult = Some(a)
                }
              }
            }
          }
        }

        def hasNext = {
          if (nextResult.nonEmpty) {
            true
          } else {
            while (underlying.hasNext && nextResult.isEmpty) {
              processOne
            }
            nextResult.nonEmpty
          }
        }

        def next = {
          val result = nextResult.get
          nextResult = None
          result
        }
      }
    }

    def lazilyGroupByEquivalenceAndInvariant[B <% Ordered[B]](equivalence: (A, A) => Boolean, invariant: A => B): Iterable[Iterator[A]] = {
      def equivalenceClasses(y: Iterator[A]): Iterator[Iterator[A]] = new Iterator[Iterator[A]] {
        import scala.collection.mutable.Queue
        import scala.collection.mutable.Map
        val nextQueues = Queue[Queue[A]]()
        val allQueues = Map[A, Queue[A]]()

        def processOne {
          val a = y.next
          allQueues.find { p => equivalence(p._1, a) } match {
            case None => {
              val newQueue = Queue(a)
              nextQueues.enqueue(newQueue)
              allQueues.put(a, newQueue)
            }
            case Some((_, aQueue)) => {
              aQueue.enqueue(a)
            }
          }
        }

        class QueueWrapper(q: Queue[A]) extends Iterator[A] {
          def hasNext = {
            if (q.nonEmpty) {
              true
            } else {
              while (y.hasNext && q.isEmpty) {
                processOne
              }
              q.nonEmpty
            }
          }
          def next = q.dequeue
        }

        def hasNext = {
          if (nextQueues.nonEmpty) {
            true
          } else {
            while (y.hasNext && nextQueues.isEmpty) {
              processOne
            }
            nextQueues.nonEmpty
          }
        }

        def next = new QueueWrapper(nextQueues.dequeue)
      }

      (x.lazilyGroupBy(invariant).map { p => equivalenceClasses(p._2) }).flatten
    }
  }

  implicit def customGroupBy[A](collection: Traversable[A]) = new CustomGroupBy(collection)

  class CustomGroupBy[A](collection: Traversable[A]) {
    def groupByWithCustomBuilder[K, CC](f: A => K)(implicit newBuilder: () => scala.collection.mutable.Builder[A, CC]): scala.collection.immutable.Map[K, CC] = {
      val m = scala.collection.mutable.Map.empty[K, scala.collection.mutable.Builder[A, CC]]
      for (elem <- collection) {
        val key = f(elem)
        val bldr = m.getOrElseUpdate(key, newBuilder())
        bldr += elem
      }
      val b = Map.newBuilder[K, CC]
      for ((k, v) <- m)
        b += ((k, v.result))

      b.result
    }
  }

}