package net.tqft.toolkit.collections
import scala.util.Random

object RandomSample {
  implicit class RandomSampleable[A](i: Iterable[A]) {
    def randomSample(d: Double): Iterable[A] = {
      require(0 <= d && d <= 1)
      new Iterable[A] {
        val seed = new Random().nextLong
        def iterator = new Iterator[A] {
          val iter = i.iterator
          val random = new Random(seed)
          var skipped = false
          def hasNext = {
            if (!skipped) {
              while (random.nextDouble > d && iter.hasNext) {
                iter.next
              }
              if (iter.hasNext) skipped = true
            }
            skipped
          }
          def next = {
            if (hasNext) {
              skipped = false
              iter.next
            } else {
              throw new NoSuchElementException
            }
          }
        }
      }
    }
  }
}