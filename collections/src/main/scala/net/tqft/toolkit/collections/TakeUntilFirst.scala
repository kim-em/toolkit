package net.tqft.toolkit.collections

object TakeUntilFirst {
  implicit def takeUntilFirst[A](x: Iterable[A]) = new TakeUntilFirstable(x)
  class TakeUntilFirstable[A](x: Iterable[A]) {
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
}
