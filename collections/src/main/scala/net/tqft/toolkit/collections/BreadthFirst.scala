package net.tqft.toolkit.collections
import net.tqft.toolkit.Throttle

object BreadthFirst {

  implicit def iterable2ToBreadthFirst[A](iterables: Iterable[Iterable[A]]) = new BreadthFirst2(iterables)
  implicit def iterable3ToBreadthFirst[A](iterables: Iterable[Iterable[Iterable[A]]]) = new BreadthFirst3(iterables)

  class BreadthFirst2[A](iterables: Iterable[Iterable[A]]) {
    import FlexibleTranspose._

    /** breadthFirstFlatten does a breadth-first flatten */
    def breadthFirstFlatten: Iterable[A] = {
      iterables.flexibleTranspose.flatten
    }

    /**
     * breadthFirstSearch successively queries each Iterable[A], resetting to the first one each time it finds something satisfying the predicate
     *
     * Example:
     * 	Suppose I = ((1,4,7,...),(2,5,8,...),(3,6,9,...)) and predicate(x) = x % 2 == 0
     * 	Then I.breadthFirstSearch(predicate) = (Some(2), Some(4), None, Some(10), Some(8), Some(16), Some(6), Some(22), Some(14), Some(28), None, ...)
     */
    def breadthFirstSearch(predicate: A => Boolean): Iterable[Option[A]] = {
      iterables.flexibleTranspose.map(_.find(predicate))
    }
  }

  class BreadthFirst3[A](iterables: Iterable[Iterable[Iterable[A]]]) {
    def breathFirstSearchNonEmpty = iterables.breadthFirstSearch(_.nonEmpty).flatten
    def breathFirstSearchNonEmptyThrottled(throttle: Throttle) = iterables.breadthFirstSearch(_.nonEmpty) flatMap { o => { throttle(o.nonEmpty); o } }
  }
}

