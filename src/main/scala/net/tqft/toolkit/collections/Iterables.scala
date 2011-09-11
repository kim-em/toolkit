package net.tqft.toolkit.collections
import scala.actors.Actor

object Iterables {
  implicit def iterable2RichIterable[A](iterable: Iterable[A]) = new RichIterable(iterable)

  class RichIterable[A](iterable: Iterable[A]) {
    def consume(f: A => Unit, numberOfWorkers: Int = 1): List[Actor] = {
      import Iterators._
      iterable.iterator.consume(f, numberOfWorkers)
    }

    def asFunction: (() => Option[A]) = {
      import Iterators._
      iterable.iterator.asFunction
    }
  }
}