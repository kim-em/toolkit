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
	  def refineByPartialFunction[U: Ordering](pf: PartialFunction[T, U]): Ordering[T] = new Ordering[T] {
	    def compare(x: T, y: T) = {
	      ord.compare(x, y) match {
	        case 0 => {
	          if(pf.isDefinedAt(x) && pf.isDefinedAt(y)) {
	            implicitly[Ordering[U]].compare(pf(x), pf(y))
	          } else {
	            0
	          }
	        }
	        case k => k
	      }
	    }
	  }
	}
}