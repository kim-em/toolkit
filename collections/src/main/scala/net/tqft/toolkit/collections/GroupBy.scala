package net.tqft.toolkit.collections

import scala.collection.mutable.ListBuffer
import scala.collection.GenIterable
import scala.collection.parallel.ParIterable
import scala.collection.GenSeq
import scala.collection.parallel.ParSeq
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConversions
import net.tqft.toolkit.Logging

object GroupBy {

  implicit def groupable[A](x: GenIterable[A]) = new Groupable(x)
  class Groupable[A](x: GenIterable[A]) {

    def equivalenceClasses(equivalence: (A, A) => Boolean): List[List[A]] = equivalenceClasses[Unit](equivalence, { x => () })

    def equivalenceClasses[B <% Ordered[B]](equivalence: (A, A) => Boolean, invariant: A => B): List[List[A]] = {
      def _equivalenceClasses(y: Seq[A]) = {
        @scala.annotation.tailrec
        def acc(classes: ParSeq[List[A]])(z: Seq[A]): Seq[List[A]] = z match {
          case Seq() => classes.seq
          case a +: r => classes.indexWhere(c => equivalence(c.head, a)) match {
            case -1 => acc(List(a) +: classes)(r)
            case k => acc(classes.updated(k, a :: classes(k)))(r)
          }
        }

        acc(ParSeq())(y)
      }

      import Split._
      ((x.toList sortBy { invariant } splitBy { invariant }).par.map { _equivalenceClasses }).toList.flatten
    }

    def chooseEquivalenceClassRepresentatives(equivalence: (A, A) => Boolean): List[A] = chooseEquivalenceClassRepresentatives[Unit](equivalence, { x => () })
    def chooseEquivalenceClassRepresentatives[B <% Ordered[B]](equivalence: (A, A) => Boolean, invariant: A => B): List[A] = {      
      def _chooseEquivalenceClassRepresentatives(y: Seq[A]) = {
        @scala.annotation.tailrec
        def acc(representatives: ParSeq[A])(z: Seq[A]): Seq[A] = z match {
          case Seq() => representatives.seq
          case a +: r => representatives.indexWhere(c => equivalence(c, a)) match {
            case -1 => acc(a +: representatives)(r)
            case k => acc(representatives)(r)
          }
        }

        acc(ParSeq())(y)
      }

      import Split._
      
      val xSeq = x.toIndexedSeq
//      Logging.info("running chooseEquivalenceClassRepresentatives on " + xSeq.size + " elements")
      ((xSeq sortBy { invariant } splitBy { invariant }).par.map { _chooseEquivalenceClassRepresentatives }).toList.flatten
    }

    def lazilyGroupBy[B](f: A => B): Iterable[(B, Iterator[A])] = {
      import scala.collection.mutable.Queue
      import scala.collection.mutable.Map

      new Iterable[(B, Iterator[A])] {
        def iterator = new Iterator[(B, Iterator[A])] {
          val underlying = x.iterator
          val allQueues = Map[B, Queue[A]]()
          val nextQueues = Queue[(B, Queue[A])]()

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

//    def parallelChooseRepresentatives[B](equivalence: (A, A) => Boolean, invariant: A => B): List[A] = {
//      val underlying = x.iterator
//      def newMap = com.google.common.collect.Sets.newSetFromMap[A](new ConcurrentHashMap[A, java.lang.Boolean]());
//      val previous = new com.google.common.collect.MapMaker().makeMap[B, java.util.Set[A]]()
//      import JavaConversions._
//      import net.tqft.toolkit.collections.Iterators._
//      val actors = underlying.consume({ a: A =>
//        {
//          val b = invariant(a)
//          previous.putIfAbsent(b, newMap)
//          val s = previous.get(b)
//          s.find(_a => equivalence(a, _a)) match {
//            case Some(_) => ()
//            case None => {
//              s += a
//            }
//          }
//        }
//      }, 16)
//
//      for (a <- actors) {
//        a !? 'finished_? match {
//          case 'done =>
//        }
//      }
//
//      val initialCut = previous.values.flatMap(x => x).toList
//      Logging.info("initial cut: " + initialCut.size)
//      val result = new Groupable(initialCut).lazilyChooseRepresentatives(equivalence, invariant).toList
//      Logging.info("final size: " + result.size)
//      result
//    }

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