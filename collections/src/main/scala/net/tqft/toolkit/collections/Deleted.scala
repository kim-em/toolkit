package net.tqft.toolkit.collections

object Deleted {
  implicit def toDeletedable[A](s: Seq[A]) = new Deletedable(s)

  class Deletedable[A](s: Seq[A]) {
    def deleted(k: Int) = s.take(k) ++ s.drop(k + 1)
    def deleted(ks: Seq[Int]) = s.zipWithIndex.collect({ case (a, k) if !ks.contains(ks) => a })
  }
}