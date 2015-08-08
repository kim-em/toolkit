package net.tqft.toolkit.collections

import scala.collection.mutable.ListBuffer
object TakeDistinct {
	implicit class TakeDistinctable[A](x: Iterable[A]) {
	  def takeDistinct: List[A] = {
		val y = new ListBuffer[A]
		x.takeWhile { a => if(y.contains(a)) { false } else { y += a; true } }.toList
	  }
	}
}