package net.tqft.toolkit.orderings

object Orderings {
  implicit class RefineByable[T](ord: Ordering[T]) {
    def refineAlong(other: Ordering[T]): Ordering[T] = new Ordering[T] {
      def compare(x: T, y: T) = {
        ord.compare(x, y) match {
          case 0 => other.compare(x, y)
          case k => k
        }
      }      
    }
    def refineBy[U: Ordering](f: T => U): Ordering[T] = refineAlong(Ordering.by(f))
    def refineByPartialFunction[U: Ordering](pf: PartialFunction[T, U]): Ordering[T] = new Ordering[T] {
      def compare(x: T, y: T) = {
        ord.compare(x, y) match {
          case 0 => {
            if (pf.isDefinedAt(x) && pf.isDefinedAt(y)) {
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