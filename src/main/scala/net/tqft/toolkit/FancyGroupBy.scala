package net.tqft.toolkit

import scala.collection.mutable.ListBuffer
object FancyGroupBy {

  implicit def groupable[A](x: Iterable[A]) = new Groupable(x)
  class Groupable[A](x: Iterable[A]) {
    def fancyGroupBy[B <% Ordered[B]](invariant: A => B, equivalence: (A, A) => Boolean): List[List[A]] = {
      def equivalenceClasses(y: List[A]) = {
        def acc(classes: List[List[A]])(z: List[A]): List[List[A]] = z match {
          case Nil => classes.toList
          case a :: r => classes.indexWhere(c => equivalence(c.head, a)) match {
            case -1 => acc(List(a) :: classes)(r)
            case k => acc(classes.updated(k, a :: classes(k)))(r)
          }
        }
        
        acc(List())(y)
      }
      
      import SplitBy._
      ((x.toList sortBy { invariant } splitBy { invariant }).par.map { equivalenceClasses }).toList.flatten
    }
  }
}