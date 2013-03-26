package net.tqft.toolkit.collections

object CartesianProduct {
  implicit def toProductable[A](sets: Seq[Traversable[A]]) = new productable(sets)

  class productable[A](sets: Seq[Traversable[A]]) {
    def cartesianProduct: Iterator[List[A]] = sets.reverse.foldLeft(Iterator[List[A]](Nil))((x, y) => for (a <- x; b <- y) yield b +: a)

    def blockCartesianProduct: Iterator[IndexedSeq[A]] = {
      val isets = sets.map(_.toIndexedSeq).toIndexedSeq
      if (isets.exists(_.isEmpty)) {
        Iterator.empty
      } else {
        val indices = isets.map(s => 0 until s.size)
        new Iterator[IndexedSeq[A]] {
          private var blockStack: List[IndexedSeq[Range]] = indices :: Nil
          override def hasNext = blockStack.nonEmpty
          @scala.annotation.tailrec
          override def next = {
            val maxBlockDimension = blockStack.head.view.map(_.size).max
            if(maxBlockDimension == 1) {
              val result = for((s, r) <- isets.zip(blockStack.head)) yield s(r.head)
              blockStack = blockStack.tail
              result
            } else {
              val position = blockStack.head.indexWhere(_.size == maxBlockDimension)
              val block1 = blockStack.head.updated(position, blockStack.head(position).take(maxBlockDimension / 2))
              val block2 = blockStack.head.updated(position, blockStack.head(position).drop(maxBlockDimension / 2))
              blockStack = block1 :: block2 :: blockStack.tail
              next
            }
          }
        }
      }
    }
  }
}