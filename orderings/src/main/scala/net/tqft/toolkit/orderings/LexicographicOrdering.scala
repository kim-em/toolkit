package net.tqft.toolkit.orderings

import scala.language.implicitConversions
import scala.collection.SortedMap

object LexicographicOrdering {

  implicit def ordering[A: Ordering, B: Ordering]: Ordering[Map[A, B]] = {
    // FIXME this could be made more efficient, probably!
    require(implicitly[Ordering[A]] != null)
    require(implicitly[Ordering[B]] != null)
    
    import Ordering.Implicits._
    Ordering.by[Map[A,B], Seq[(A, B)]]({ x: Map[A, B] => (SortedMap[A, B]() ++ x).toSeq })
  }

}