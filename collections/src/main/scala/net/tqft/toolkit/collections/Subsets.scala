package net.tqft.toolkit.collections

object Subsets {

  implicit class Subsetable[A](list: Seq[A]) {
    def subsets: Iterator[Set[A]] = {
      if (list.size == 0) {
        Iterator(list.toSet)
      } else {
        val tailSubsets = new Subsetable(list.tail).subsets
        tailSubsets ++ tailSubsets.map(_ + list.head)
      }
    }
  }
}