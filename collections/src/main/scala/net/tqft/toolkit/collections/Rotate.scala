package net.tqft.toolkit.collections

object Rotate {
  implicit class Rotatable[A](i: Seq[A]) {

    // from net.tqft.toolkit.arithmetic.Mod, for the sake of avoiding the dependency
    private def mod(x: Int, k: Int) = {
      if (x < 0) {
        -((-x - 1) % k) + k - 1
      } else {
        x % k
      }
    }

    def rotateLeft(k: Int) = {
      if (i.size == 0) {
        i
      } else {
        val m = mod(k, i.size)
        i.drop(m) ++ i.take(m)
      }
    }
    def rotateRight(k: Int) = {
      if (i.size == 0) {
        i
      } else {
        val m = mod(k, i.size)
        i.takeRight(m) ++ i.dropRight(m)
      }
    }

    def leastRotation(implicit ordering: Ordering[Seq[A]]) = {
      if (i.isEmpty) {
        i
      } else {
        (for (k <- 0 until i.size) yield rotateLeft(k)).min
      }
    }
  }

}