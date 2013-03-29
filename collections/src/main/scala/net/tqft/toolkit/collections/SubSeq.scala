package net.tqft.toolkit.collections

object SubList {
	implicit def subseqable[A](x: Seq[A]) = new SubSeqable(x)
	class SubSeqable[A](x: Seq[A]) {
	  def isSubSeqOf(y: Seq[A]): Boolean = {
		 x match {
		   case Seq() => true
		   case h +: t => y.contains(h) && (new SubSeqable(t).isSubSeqOf(y.drop(y.indexOf(h) + 1)))
		 }
	  }
	  def isCyclicSubSeqOf(y: Seq[A]): Boolean = {
	    import Rotate._
	    (for(i <- 0 to y.size - 1; ry = y.rotateLeft(i); if x isSubSeqOf ry) yield ry).nonEmpty
	  }
	}
}