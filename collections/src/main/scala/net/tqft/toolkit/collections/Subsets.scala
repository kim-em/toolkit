package net.tqft.toolkit.collections

object Subsets {

  implicit def subsets[A](list: Seq[A]) = new Subsetable(list)

  class Subsetable[A](list: Seq[A]) {
    def subsets: Iterable[Set[A]] = {
      if (list.size == 0) {
        Iterable(list.toSet)
      } else {
        val tailSubsets = new Subsetable(list.tail).subsets
        tailSubsets ++ tailSubsets.map(_ + list.head)
      }
    }
  }
}