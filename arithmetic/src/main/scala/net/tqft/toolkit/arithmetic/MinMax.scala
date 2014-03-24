package net.tqft.toolkit.arithmetic

import scala.language.implicitConversions


object MinMax {

  implicit class MinMaxIterable[A](iterable: Iterable[A]) {
    def minOption(implicit cmp: Ordering[A]): Option[A] = if(iterable.isEmpty) {
      None
    } else {
      Some(iterable.min)
    }
    def maxOption(implicit cmp: Ordering[A]): Option[A] = if(iterable.isEmpty) {
      None
    } else {
      Some(iterable.max)
    }
  }
  
}