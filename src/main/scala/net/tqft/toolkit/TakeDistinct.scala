package net.tqft.toolkit

import scala.collection.mutable.ListBuffer
object TakeDistinct {
	implicit def takeDistinctable[A](x: Iterable[A]) = new TakeDistinctable(x)
	class TakeDistinctable[A](x: Iterable[A]) {
	  def takeDistinct: List[A] = {
		val y = new ListBuffer[A]
		x.takeWhile { a => if(y.contains(a)) { false } else { y += a; true } }.toList
	  }
	}
}