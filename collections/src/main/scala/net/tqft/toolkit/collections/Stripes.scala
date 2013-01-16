package net.tqft.toolkit.collections

object Stripes {

  import scala.collection.mutable.ListBuffer
  implicit def stripes[A](collection: Traversable[A]) = new Stripes(collection)

  class Stripes[A](collection: Traversable[A]) {
    def stripes(k: Int): List[List[A]] = {
      val builders = List.fill(k)(new ListBuffer[A]())
      var i = 0
      for (a <- collection) {
        builders(i) += a
        i = (i + 1) % k
      }
      builders map (_.toList)
    }
  }

}