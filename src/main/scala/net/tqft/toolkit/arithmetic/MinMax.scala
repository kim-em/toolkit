package net.tqft.toolkit.arithmetic

object MinMax {

  implicit def toMinMaxList[A](list: List[A]) = new MinMaxList(list)
  
  class MinMaxList[A](list: List[A]) {
    def minOption(implicit cmp: Ordering[A]): Option[A] = list match {
      case Nil => None
      case _ => Some(list.min)
    }
    def maxOption(implicit cmp: Ordering[A]): Option[A] = list match {
      case Nil => None
      case _ => Some(list.max)
    }
  }
  
}