package net.tqft.toolkit.collections

object Split {

  implicit def splittable[A](x: List[A]) = new Splittable(x)
  class Splittable[A](x: List[A]) {
    def splitBy[B](f: A => B): List[List[A]] = {
      def chunk(l: List[(A, B)]): List[List[A]] = {
        if (l.nonEmpty) {
          val (c, rest) = l.span(_._2 == l.head._2)
          c.map(_._1) :: chunk(rest)
        } else {
          Nil
        }
      }

      chunk(x map (a => (a, f(a))))
    }
    
    def split = splitBy(x => x)
    def rle = split.map(l => (l.head, l.size))
  }

}

