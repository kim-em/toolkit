package net.tqft.toolkit.collections
import scala.collection.mutable.ListBuffer

object CachingIterable {

  def apply[A](i: Iterable[A]): Iterable[A] = apply(i.iterator)
  def apply[A](i: Iterator[A]): Iterable[A] = new NonStrictIterable[A] {
    private[this] val cache = ListBuffer[A]()

    private[this] def cacheOneMore {
      synchronized {
        if (i.hasNext) {
          cache += i.next
        }
      }
    }

    def iterator = new Iterator[A] {
      var k = 0

      def hasNext = {
        while (k >= cache.size && i.hasNext) {
          cacheOneMore
        }
        k < cache.size
      }

      def next = {
        val result = cache(k)
        k = k + 1
        result
      }
    }
  }
}