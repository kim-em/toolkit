package net.tqft.toolkit

import scala.collection.mutable.ListBuffer
import scala.collection.GenIterable
import scala.collection.parallel.ParIterable
import scala.collection.GenSeq
import scala.collection.parallel.ParSeq
object GroupBy {

  implicit def groupable[A](x: GenIterable[A]) = new Groupable(x)
  class Groupable[A](x: GenIterable[A]) {

    def groupByEquivalence(equivalence: (A, A) => Boolean): List[List[A]] = groupByEquivalence[Unit](equivalence, { x => () })

    def groupByEquivalence[B <% Ordered[B]](equivalence: (A, A) => Boolean, invariant: A => B): List[List[A]] = {
      def equivalenceClasses(y: List[A]) = {
        def acc(classes: ParSeq[List[A]])(z: List[A]): List[List[A]] = z match {
          case Nil => classes.toList
          case a :: r => classes.indexWhere(c => equivalence(c.head, a)) match {
            case -1 => acc(List(a) +: classes)(r)
            case k => acc(classes.updated(k, a :: classes(k)))(r)
          }
        }

        acc(Nil.par)(y)
      }

      import SplitBy._
      ((x.toList sortBy { invariant } splitBy { invariant }).par.map { equivalenceClasses }).toList.flatten
    }
  }

  implicit def customGroupBy[A](collection: Traversable[A]) = new CustomGroupBy(collection)

  class CustomGroupBy[A](collection: Traversable[A]) {
    def groupByWithCustomBuilder[K, CC](f: A => K)(implicit newBuilder: () => scala.collection.mutable.Builder[A, CC]): scala.collection.immutable.Map[K, CC] = {
      val m = scala.collection.mutable.Map.empty[K, scala.collection.mutable.Builder[A, CC]]
      for (elem <- collection) {
        val key = f(elem)
        val bldr = m.getOrElseUpdate(key, newBuilder())
        bldr += elem
      }
      val b = Map.newBuilder[K, CC]
      for ((k, v) <- m)
        b += ((k, v.result))

      b.result
    }
  }

}