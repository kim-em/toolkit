package net.tqft.toolkit.collections

object Rotate {
  implicit class Rotatable[A](i: Seq[A]) {
    def rotateLeft(k: Int) = {
      if (i.size == 0) {
        i
      } else {
        val m = k % i.size
        i.drop(m) ++ i.take(m)
      }
    }
    def rotateRight(k: Int) = {
      if (i.size == 0) {
        i
      } else {
        val m = k % i.size
        i.takeRight(m) ++ i.dropRight(m)
      }
    }
  }

}