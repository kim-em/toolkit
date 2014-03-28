package net.tqft.toolkit.collections

object TakeUntilFirst {
  implicit class TakeUntilFirstable[A](x: Iterable[A]) {
    def takeUntilFirst(condition: A => Boolean): Iterable[A] = {
      new Iterable[A] {
        def iterator = new Iterator[A] {
          val inner = x.iterator
          var found = false
          def hasNext = !found && inner.hasNext
          def next = {
            val result = inner.next
            if (condition(result)) {
              found = true
            }
            result
          }
        }
      }
    }
  }
  
  implicit class TakeUntilFirstableIterator[A](x: Iterator[A]) {
    def takeUntilFirst(condition: A => Boolean): Iterator[A] = {
      new Iterator[A] {
          var found = false
          def hasNext = !found && x.hasNext
          def next = {
            val result = x.next
            if (condition(result)) {
              found = true
            }
            result
          }
        }
    }
  }
}
