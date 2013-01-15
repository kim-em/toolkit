package net.tqft.toolkit.collections

object Orderings {
	implicit def toRefineByable[T](ord: Ordering[T]): RefineByable[T] = new RefineByable[T](ord)
	
	class RefineByable[T](ord: Ordering[T]) {
	  def refineBy[U:Ordering](f: T => U): Ordering[T] = new Ordering[T] {
	    def compare(x: T, y:T) = {
	      ord.compare(x, y) match {
	        case 0 => implicitly[Ordering[U]].compare(f(x), f(y))
	        case k => k
	      }
	    }
	  }
	}
}