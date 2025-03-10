package net.tqft.toolkit.collections

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.collection.parallel.ParIterable
import scala.collection.parallel.IterableSplitter
import scala.collection.GenIterable

object NonStrictIterable {
  def apply[A](s: A*): Iterable[A] = from(s)

  def empty[A] = apply()
  
  def from[A](i: Iterable[A]): Iterable[A] = {
    if (i.isInstanceOf[NonStrictIterable[_]]) {
      i
    } else {
      new NonStrictIterable[A] {
        def iterator = i.iterator
      }
    }
  }

  def iterate[A](a: => A)(f: A => A): Iterable[A] = {
    new NonStrictIterable[A] {
      def iterator = Iterator.iterate(a)(f)
    }
  }

  def iterateUntilNone[A](a: => A)(f: A => Option[A]): Iterable[A] = {
    iterate[Option[A]](Some(a))({
      case Some(a) => f(a)
      case None => None
    }).takeWhile({ case Some(_) => true; case None => false }).collect({ case Some(b) => b })
  }

  def continually[A](f: => A): Iterable[A] = new NonStrictIterable[A] {
    def iterator = Iterator.continually(f)
  }
}

trait NonStrictIterable[A] extends Iterable[A] { self =>
  def iterator: Iterator[A]

  override def tail = {
    new NonStrictIterable[A] {
      def iterator = {
        val i = self.iterator
        i.next
        i
      }
    }
  }

  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Iterable[A], B, That]): That = {
    new NonStrictIterable[B] {
      def iterator = self.iterator map f
    }.asInstanceOf[That]
  }

  override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Iterable[A], B, That]): That = {
    new NonStrictIterable[B] {
      def iterator = self.iterator flatMap { a: A => f(a).toIterable.iterator }
    }.asInstanceOf[That]
  }

// Breaks in 2.10.0-SNAPSHOT  
  override def flatten[B](implicit asTraversable: A => GenTraversableOnce[B]): Iterable[B] = {
    flatMap { x: A => asTraversable(x) }
  }

  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Iterable[A], B, That]): That = {
    new NonStrictIterable[B] {
      def iterator = self.iterator ++ that.toIterable.iterator
    }.asInstanceOf[That]
  }

  override def filter(f: A => Boolean) = {
    new NonStrictIterable[A] {
      def iterator = self.iterator filter f
    }
  }

  override def take(k: Int) = {
    new NonStrictIterable[A] {
      def iterator = self.iterator take k
    }
  }

  override def takeWhile(f: A => Boolean) = {
    new NonStrictIterable[A] {
      def iterator = self.iterator takeWhile f
    }
  }

  override def headOption: Option[A] = {
    val i = self.iterator
    if (i.hasNext) {
      Some(i.next)
    } else {
      None
    }
  }

  override def find(p: A => Boolean): Option[A] = {
    self.iterator.find(p)
  }

  override def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Iterable[A], B, That]): That = {
    new NonStrictIterable[B] {
      def iterator = self.iterator collect pf
    }.asInstanceOf[That]
  }

  override def partition(p: A => Boolean): (Iterable[A], Iterable[A]) = {
    import scala.collection.mutable.Queue

    val trueIteratorQueue = Queue[Iterator[A]]()
    val falseIteratorQueue = Queue[Iterator[A]]()

    def constructNewIteratorPair {
      val underlying = self.iterator

      val trueQueue = scala.collection.mutable.Queue[A]()
      val falseQueue = scala.collection.mutable.Queue[A]()

      final class PartitionIterator(underlying: Iterator[A], p: A => Boolean, trueQueue: Queue[A], falseQueue: Queue[A]) extends Iterator[A] {
        @scala.annotation.tailrec
        def hasNext = {
          if (trueQueue.isEmpty) {
            if (underlying.hasNext) {
              val next = underlying.next
              if (p(next)) {
                trueQueue.enqueue(next)
                true
              } else {
                falseQueue.enqueue(next)
                hasNext
              }
            } else {
              false
            }
          } else {
            true
          }
        }

        def next = trueQueue.dequeue
      }

      val trueIterator = new PartitionIterator(underlying, p, trueQueue, falseQueue)
      val falseIterator = new PartitionIterator(underlying, x => !p(x), falseQueue, trueQueue)
      synchronized {
        trueIteratorQueue.enqueue(trueIterator)
        falseIteratorQueue.enqueue(falseIterator)
      }
    }

    (new NonStrictIterable[A] {
      def iterator = {
        if (trueIteratorQueue.isEmpty) constructNewIteratorPair
        trueIteratorQueue.dequeue
      }
    },
      new NonStrictIterable[A] {
        def iterator = {
          if (falseIteratorQueue.isEmpty) constructNewIteratorPair
          falseIteratorQueue.dequeue
        }
      })
  }

  override def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[Iterable[A], (A1, B), That]): That = {
    new NonStrictIterable[(A, B)] {
      def iterator = self.iterator zip that.iterator
    }.asInstanceOf[That]
  }

  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Iterable[A], (A1, Int), That]): That = zip(NonStrictNaturalNumbers)
  
  override def zipAll[B, A1 >: A, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[Iterable[A], (A1, B), That]): That = {
    new NonStrictIterable[(A1, B)] {
      def iterator = self.iterator.zipAll(that.iterator, thisElem, thatElem)
    }.asInstanceOf[That]
  }
  
  override def equals(other: Any) = {
    other match {
      case other: Iterable[_] => this.map(Some(_)).zipAll(NonStrictIterable.from(other).map(Some(_)), None, None).find {
        case (Some(x), Some(y)) if x != y => true
        case (Some(_), None) => true
        case (None, Some(_)) => true
        case _ => false
      }.nonEmpty
      case _ => false
    }
  }
  
}