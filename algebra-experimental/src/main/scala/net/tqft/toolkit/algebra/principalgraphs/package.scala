package net.tqft.toolkit.algebra

package object principalgraphs {
	implicit class RichSeq[A](x: Seq[A]) {
	  def most = x.dropRight(1)
	  def removed(i: Int) = x.take(i) ++ x.drop(i + 1)
	  def secondLast = x(x.size - 2)
	}
	
	implicit class RichBoolean(x: Boolean) {
	  def option[A](a: => A) = if(x) {
	    Some(a)
	  } else {
	    None
	  }
	}
}