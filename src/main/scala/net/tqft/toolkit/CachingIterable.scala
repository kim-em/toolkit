package net.tqft.toolkit
import scala.collection.mutable.ListBuffer

object CachingIterable {

  def apply[A](i: Iterable[A]): Iterable[A] = new NonStrictIterable[A] {
    private[this] val cache = ListBuffer[A]()
    private[this] val oneIterator = i.iterator

    private[this] def cacheOneMore {
      cache += oneIterator.next
    }

    def iterator = new Iterator[A] {
      var k = 0

      def hasNext = {
        while (k >= cache.size && oneIterator.hasNext) {
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