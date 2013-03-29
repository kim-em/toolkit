package net.tqft.toolkit.collections

object TakeToFirst {
  implicit def takeToFirst[A](x: Iterator[A]) = new TakeToFirstable(x)
  class TakeToFirstable[A](x: Iterator[A]) {
    def takeToFirst(condition: A => Boolean): List[A] = {
      var found = false
      val lb = new scala.collection.mutable.ListBuffer[A]
      while (!found && x.hasNext) {
        val n = x.next
        if (condition(n)) found = true
        lb += n
      }
      lb.toList
    }
  }  
}