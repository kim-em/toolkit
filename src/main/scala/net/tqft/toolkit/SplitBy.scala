package net.tqft.toolkit

object SplitBy {

  implicit def splittable[A](x: List[A]) = new Splittable(x)
  class Splittable[A](x: List[A])  {
    def splitBy[B](f: A => B) = {
      def chunk(l: List[(A, B)]): List[List[A]] = l match {
        case h :: r => {
          val k = l.indexWhere(_._2 != h._2)
          k match {
            case -1 => List(l map (_._1))
            case k => (l.take(k) map (_._1)) :: chunk(l.drop(k))
          }
        }
        case Nil => List()
      }
      
      chunk(x map (a => (a, f(a))))
    }
  }
}