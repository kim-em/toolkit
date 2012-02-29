package net.tqft.toolkit.collections
import scala.actors.Actor
import scala.collection.generic.CanBuildFrom
import scala.actors.IScheduler

object Iterables {
  implicit def iterable2RichIterable[A](iterable: Iterable[A]) = new RichIterable(iterable)

  class RichIterable[A](iterable: Iterable[A]) {
    def mapWhileDefined[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Iterable[A], B, That]): That = {
      new NonStrictIterable[B] {
        import Iterators._
        def iterator = iterable.iterator mapWhileDefined pf
      }.asInstanceOf[That]
    }

    def consume(f: A => Unit, numberOfWorkers: Int = 1): List[Actor] = {
      import Iterators._
      iterable.iterator.consume(f, numberOfWorkers)
    }

    def asFunction: (() => Option[A]) = {
      import Iterators._
      iterable.iterator.asFunction
    }

    def findMinimum[B <% Ordered[B]](f: A => B, lowerBound: Option[B] = None): A = {
      import Iterators._
      iterable.iterator.findMinimum(f, lowerBound)
    }

  }
}