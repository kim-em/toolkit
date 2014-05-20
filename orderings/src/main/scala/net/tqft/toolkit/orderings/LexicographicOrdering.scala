package net.tqft.toolkit.orderings

import scala.language.implicitConversions
import scala.collection.SortedMap

object LexicographicOrdering {

  implicit def mapOrdering[A: Ordering, B: Ordering]: Ordering[Map[A, B]] = {
    // FIXME this could be made more efficient, probably!
    require(implicitly[Ordering[A]] != null)
    require(implicitly[Ordering[B]] != null)

    new Ordering[Map[A, B]] {
      override def compare(x: Map[A, B], y: Map[A, B]) = {
        val keys = (x.keys ++ y.keys).toSeq.sorted.reverse
//        println(s"keys = $keys")
        val xs = keys.map(x.get)
        val ys = keys.map(y.get)
        import Ordering.Implicits._
        implicitly[Ordering[Seq[Option[B]]]].compare(xs, ys)
      }
    }
  }

}