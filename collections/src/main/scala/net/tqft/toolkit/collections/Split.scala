package net.tqft.toolkit.collections

object Split {

  // TODO replace Iterable with a generic CC
  implicit def splittable[A](x: Iterable[A]) = new Splittable(x)
  class Splittable[A](x: Iterable[A]) {
    // TODO should return an Iterable
    def splitBy[B](f: A => B): List[Seq[A]] = {
      def chunk(l: Seq[(A, B)]): List[Seq[A]] = {
        if (l.nonEmpty) {
          val (c, rest) = l.span(_._2 == l.head._2)
          c.map(_._1) :: chunk(rest)
        } else {
          Nil
        }
      }

      chunk(x.toSeq map (a => (a, f(a))))
    }
    
    def splitByOrdering(o: Ordering[A]): List[List[A]] = {
      val sorted = x.toList.sorted(o)
      def chunk(l: List[A]): List[List[A]] = {
        l match {
          case h :: t => {
            val (c, rest) = t.span(o.compare(_, h) == 0)
            (h :: c) :: chunk(rest)
          }
          case Nil => Nil
        }
      }
      chunk(sorted)
    }
    
    def split = splitBy(x => x)
    def rle = split.map(l => (l.head, l.size))

    def splitAfter(p: A => Boolean): Iterable[List[A]] = {
      new NonStrictIterable[List[A]] {
        val i = x.iterator
        def iterator: Iterator[List[A]] = new Iterator[List[A]] {
          def hasNext = i.hasNext
          def next = {
            import TakeToFirst._
            i.takeToFirst(p)
          }
        }
      }
    }
    
    def splitOn(p: A => Boolean) = splitAfter(p).map(s => if(p(s.last)) s.init else s)
  }

}

