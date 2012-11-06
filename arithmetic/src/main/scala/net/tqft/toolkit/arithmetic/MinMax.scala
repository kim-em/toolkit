package net.tqft.toolkit.arithmetic

object MinMax {

  implicit def toMinMaxIterable[A](iterable: Iterable[A]) = new MinMaxIterable(iterable)
  
  class MinMaxIterable[A](iterable: Iterable[A]) {
    def minOption(implicit cmp: Ordering[A]): Option[A] = iterable match {
      case Nil => None
      case _ => Some(iterable.min)
    }
    def maxOption(implicit cmp: Ordering[A]): Option[A] = iterable match {
      case Nil => None
      case _ => Some(iterable.max)
    }
  }
  
}