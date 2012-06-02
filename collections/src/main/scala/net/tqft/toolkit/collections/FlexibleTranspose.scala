package net.tqft.toolkit.collections

import scala.collection.mutable.ListBuffer
object FlexibleTranspose {
  implicit def flexibleTranspose[A](listOfLists: List[List[A]]) = new FlexibleTranspose(listOfLists)
  implicit def flexibleTransposeIterables[A](iterableOfIterables: Iterable[Iterable[A]]) = new FlexibleTransposeIterables(iterableOfIterables)

  class FlexibleTranspose[A](listOfLists: List[List[A]]) {
    def flexibleTranspose: List[List[A]] = {
      val builders = Stream.continually(new ListBuffer[A]())
      for (list <- listOfLists) {
        for ((b, a) <- builders zip list) {
          b += a
        }
      }
      builders.takeWhile(_.nonEmpty) map (_.toList) toList
    }
  }

  class FlexibleTransposeIterables[A](iterableOfIterables: Iterable[Iterable[A]]) {
    def flexibleTranspose: Iterable[Iterable[A]] = {
      new NonStrictIterable[Iterable[A]] {
        def iteratorIterator: Iterator[Iterator[A]] = new Iterator[Iterator[A]] {
          val iterableOfIterators = CachingIterable(iterableOfIterables.map(_.iterator))
          def hasNext = true
          def next: Iterator[A] = {
            val inner = iterableOfIterators.iterator
            if (!inner.hasNext) {
              Iterator.empty
            } else {
              new Iterator[A] {
                var currentIteratorOption: Option[Iterator[A]] = Some(inner.next)
                def hasNext = {
                  while ((currentIteratorOption.isEmpty || !currentIteratorOption.get.hasNext) && inner.hasNext) {
                    currentIteratorOption = Some(inner.next)
                  }
                  currentIteratorOption.map(_.hasNext).getOrElse(false)
                }
                def next = {
                  val result = currentIteratorOption.get.next
                  currentIteratorOption = None
                  result
                }
              }
            }
          }
        }
        def iterator = iteratorIterator map { CachingIterable(_) }
      }
    }
  }
}