package net.tqft.toolkit.algebra.spiders

object PlanarPartitions {

  private def impl1(size: Integer, allowSingletons: Boolean): Stream[Seq[Seq[Int]]] = {
    impl2((0 until size).toList, allowSingletons)
  }

  private val cached = {
    import net.tqft.toolkit.functions.Memo
    Memo(impl1 _)
  }

  def apply(size: Integer, allowSingletons: Boolean = false): Stream[Seq[Seq[Int]]] = cached(size, allowSingletons)
  def of[A](elements: Seq[A], allowSingletons: Boolean = false): Iterator[Seq[Seq[A]]] = {
    val indexedElements = elements.toIndexedSeq
    for (p <- apply(indexedElements.size, allowSingletons).iterator) yield {
      p.map(_.map(i => indexedElements(i)))
    }
  }

  private def powerSet[A](s: List[A]): List[List[A]] = {
    @annotation.tailrec
    def pwr(s: List[A], acc: List[List[A]]): List[List[A]] = s match {
      case Nil => acc
      case a :: as => pwr(as, acc ::: (acc map (a :: _)))
    }
    pwr(s, Nil :: Nil).map(_.reverse)
  }

  private def impl2(set: List[Int], allowSingletons: Boolean): Stream[Seq[Seq[Int]]] = {
    //println("finding planar partitions of " + set)
    import net.tqft.toolkit.collections.Split._
    import net.tqft.toolkit.collections.CartesianProduct._
    set match {
      case head :: tail => {
        (for (
          nextPart <- powerSet(tail).map(head :: _);
          parts = set.splitOn(nextPart.contains).filter(_.nonEmpty);
          if allowSingletons || nextPart.size > 1 && parts.forall(_.size > 1);
          partsPartitions = parts.map(part => of(part, allowSingletons).toIndexedSeq).toIndexedSeq;
          partition <- partsPartitions.cartesianProduct
        ) yield {
          nextPart :: partition.flatten
        }).toStream
      }
      case Nil => Stream(Seq.empty)
    }

  }
}