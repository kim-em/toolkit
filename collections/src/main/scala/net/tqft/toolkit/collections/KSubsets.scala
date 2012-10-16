package net.tqft.toolkit.collections
import net.tqft.toolkit.Logging

object KSubsets extends Logging {
  def apply(n: Int, k: Int): Iterable[Seq[Int]] = {
    new KSubsetable(0 until n) kSubsets (k)
  }

  implicit def kSubsets[A](list: Seq[A]) = new KSubsetable(list)

  class KSubsetable[A](list: Seq[A]) {
    def kSubsets(k: Int): Iterable[Seq[A]] = new NonStrictIterable[Seq[A]] {
      def iterator = {
//        info("Generating ksubsets(" + k + ") iterator for " + list)
        
        if (k > list.size) {
          None.iterator
        } else if (k == 0) {
          Some[List[A]](List()).iterator
        } else if (list.size == 1 && k == 1) {
          Some(list).iterator
        } else {
          ((new KSubsetable(list.tail).kSubsets(k)) ++ (new KSubsetable(list.tail).kSubsets(k - 1) map { l => list.head +: l })).iterator
        }
      }
    }
  }
}