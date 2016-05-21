package net.tqft.toolkit.collections

object Subsets {

  implicit class Subsetable[A](list: Seq[A]) {
    def subsets: Iterator[Seq[A]] = {
      if (list.size == 0) {
        Iterator(list)
      } else {
        val tailSubsets = new Subsetable(list.tail).subsets
        tailSubsets.flatMap(s => Iterator(list.head +: s, s))
      }
    }
  }
}