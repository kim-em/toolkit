package net.tqft.toolkit

object SubList {
	implicit def sublistable[A](x: List[A]) = new SubListable(x)
	class SubListable[A](x: List[A]) {
	  def isSubListOf(y: List[A]): Boolean = {
		 x match {
		   case Nil => true
		   case h :: t => y.contains(h) && (new SubListable(t).isSubListOf(y.drop(y.indexOf(h) + 1)))
		 }
	  }
	  def isCyclicSubListOf(y: List[A]): Boolean = {
	    import Rotate._
	    (for(i <- 0 to y.size - 1; ry = y.rotateLeft(i); if x isSubListOf ry) yield ry).nonEmpty
	  }
	}
}