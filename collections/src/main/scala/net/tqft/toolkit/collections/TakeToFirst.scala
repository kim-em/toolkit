package net.tqft.toolkit.collections

object TakeToFirst {
  implicit class TakeUntilFirstableIterator[A](x: Iterator[A]) {
    def takeToFirst(condition: A => Boolean): Iterator[A] = {
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
  
  implicit class TakeToFirstable[A](x: Iterable[A]) {
    def takeToFirst(condition: A => Boolean): Iterable[A] = {
      new Iterable[A] {
        def iterator = x.iterator.takeToFirst(condition)
      }
    }
  }
  
}
