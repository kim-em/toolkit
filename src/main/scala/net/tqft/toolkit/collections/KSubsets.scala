package net.tqft.toolkit.collections
import net.tqft.toolkit.Logging

object KSubsets extends Logging {
  def apply(n: Int, k: Int): Iterable[List[Int]] = {
    new KSubsetable((0 until n).toList) ksubsets (k)
  }

  implicit def ksubsets[A](list: List[A]) = new KSubsetable(list)

  class KSubsetable[A](list: List[A]) {
    def ksubsets(k: Int): Iterable[List[A]] = new NonStrictIterable[List[A]] {
      def iterator = {
//        info("Generating ksubsets(" + k + ") iterator for " + list)
        
        if (k > list.size) {
          None.iterator
        } else if (k == 0) {
          Some[List[A]](List()).iterator
        } else if (list.size == 1 && k == 1) {
          Some(list).iterator
        } else {
          ((new KSubsetable(list.tail).ksubsets(k)) ++ (new KSubsetable(list.tail).ksubsets(k - 1) map { l => list.head :: l })).iterator
        }
      }
    }
  }
}