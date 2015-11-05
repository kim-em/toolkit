package net.tqft.toolkit.collections

object Tally {

  implicit class Tallyable[A](i: Iterable[A]) {
    def tally: Map[A, Int] = {
      val counter = scala.collection.mutable.Map[A, Int]()
      for(a <- i) {
        if(counter.contains(a)) {
          counter(a) = counter(a) + 1
        } else {
          counter(a) = 1
        }
      }
      counter.toMap
    }
  }

}