package net.tqft.toolkit.collections

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike

object Pad {
  implicit def asPaddable[CC[+X] <: SeqLike[X, CC[X]], A](x: CC[A]) = new Paddable(x)

  class Paddable[CC[+X] <: SeqLike[X, CC[X]], A](val x: CC[A]) {
    def padLeft[B >: A](len: Int, elem: B)(implicit bf: CanBuildFrom[CC[A], B, CC[B]]): CC[B] = {
      if (x.size >= len) {
        x
      } else {
        val b = bf(x.repr)
        b.sizeHint(x.size max len)
        var diff = len - x.size
        while (diff > 0) {
          b += elem
          diff -= 1
        }
        b ++= x
        b.result
      }
    }
    def padRight[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[CC[A], B, That]): That = x.padTo(len, elem)
  }

}