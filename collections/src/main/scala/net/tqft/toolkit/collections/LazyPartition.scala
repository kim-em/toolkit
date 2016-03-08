package net.tqft.toolkit.collections

object LazyPartition {
  implicit class LazyPartitionable[A](s: Iterator[A]) {
    def lazyPartition(f: A => Boolean): (Iterator[A], Iterator[A]) = {
      val trueQueue = new scala.collection.mutable.Queue[A]
      val falseQueue = new scala.collection.mutable.Queue[A]

      def queue(p: Boolean) = if (p) trueQueue else falseQueue

      def iterator(p: Boolean) = new Iterator[A] {
        override def hasNext: Boolean = {
          if (queue(p).nonEmpty) {
            true
          } else {
            while (s.hasNext) {
              val a = s.next()
              if (f(a) == p) {
                queue(p) += a
                return true
              } else {
                queue(!p) += a
              }
            }
            false
          }
        }
        override def next = queue(p).dequeue
      }

      (iterator(true), iterator(false))
    }
  }
}