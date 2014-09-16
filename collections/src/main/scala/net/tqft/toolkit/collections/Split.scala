package net.tqft.toolkit.collections

import scala.collection.mutable.ListBuffer

object Split {

  implicit class SplittableIterator[A](x: Iterator[A]) {
    def splitBefore(p: A => Boolean): Iterator[List[A]] = {
      new Iterator[List[A]] {
        var box: Option[A] = None
        def hasNext = box.nonEmpty || x.hasNext
        def next = {
          val lb = ListBuffer[A]()
          for (b <- box) lb += b
          box = None
          if (!x.hasNext) {
            lb.toList
          } else {
            box = Some(x.next)
          }
          while (box.nonEmpty && !p(box.get)) {
            lb += box.get
            if (x.hasNext) {
              box = Some(x.next)
            } else {
              box = None
            }
          }
          lb.toList
        }
      }
    }

    def splitAfter(p: A => Boolean): Iterator[List[A]] = {
      new Iterator[List[A]] {
        def hasNext = x.hasNext
        def next = {
          import TakeToFirst._
          x.takeToFirst(p).toList
        }
      }
    }

    def splitOn(p: A => Boolean) = splitAfter(p).map(s => if (p(s.last)) s.init else s)

  }

  // TODO replace Iterable with a generic CC
  implicit class Splittable[A](x: Iterable[A]) {
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

    def splitByOrdering(o: Ordering[A]): Iterator[List[A]] = {
      val sorted = x.toVector.sorted(o)
      if (sorted.size == 0) {
        Iterator.empty
      } else if (sorted.size == 1) {
        Iterator(sorted.toList)
      } else {
        sorted.iterator.zip(sorted.sliding(2).map(p => o.compare(p(0), p(1))) ++ Iterator(0)).splitAfter(_._2 < 0).map(_.map(_._1))
      }
    }

    def split = splitBy(x => x)
    def rle = split.map(l => (l.head, l.size))

    def splitAfter(p: A => Boolean): Iterable[List[A]] = {
      new NonStrictIterable[List[A]] {
        def iterator: Iterator[List[A]] = new SplittableIterator(x.iterator).splitAfter(p)
      }
    }
    def splitBefore(p: A => Boolean): Iterable[List[A]] = {
      new NonStrictIterable[List[A]] {
        def iterator: Iterator[List[A]] = new SplittableIterator(x.iterator).splitBefore(p)
      }
    }

    def splitOn(p: A => Boolean) = splitAfter(p).map(s => if (p(s.last)) s.init else s)
  }
}

