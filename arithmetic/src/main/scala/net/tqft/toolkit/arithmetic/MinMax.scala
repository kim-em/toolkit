package net.tqft.toolkit.arithmetic

object MinMax {
  implicit class MinMaxOption[A](c: TraversableOnce[A]) {
    def minOption(implicit cmp: Ordering[A]): Option[A] = if(c.isEmpty) {
      None
    } else {
      Some(c.min)
    }
    def maxOption(implicit cmp: Ordering[A]): Option[A] = if(c.isEmpty) {
      None
    } else {
      Some(c.max)
    }
  }
}