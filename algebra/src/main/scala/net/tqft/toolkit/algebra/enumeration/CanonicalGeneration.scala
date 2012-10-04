package net.tqft.toolkit.algebra.enumeration
import net.tqft.toolkit.collections.LexicographicOrdering._

object CanonicalGeneration {

  def goodChildren[A, B: Ordering](children: A => Iterator[A], parents: A => Set[A], invariant: A => B)(a: A): Iterator[A] = {
    import Ordered._
    val b = invariant(a)

    for (
      c <- children(a);
      if parents(c).forall(p => invariant(p) >= b)
    ) yield c
  }

  def goodDescendants[A, B: Ordering](children: A => Iterator[A], parents: A => Set[A], invariant: A => B, accept: A => Boolean)(a: A): Iterator[A] = {
    if (accept(a)) {
      Iterator(a) ++ goodChildren(children, parents, invariant)(a).flatMap(goodDescendants(children, parents, invariant, accept))
    } else {
      Iterator.empty
    }
  }
}

// The type `A` stands in for an eventual concrete type.
trait Enumerable[A <: Enumerable[A, B], B] { self: A =>
  def children: Iterator[A]
  def parents: Set[A]

  def invariant: B
  implicit val ordering: Ordering[B]
  import Ordered._

  def goodChildren: Iterator[A] = {
    for (
      c <- children;
      if c.parents.forall(p => p.invariant >= invariant)
    ) yield c
  }

  def goodDescendants(accept: A => Boolean = { _ => true }): Iterator[A] = {
    if (accept(this)) {
      Iterator(this) ++ goodChildren.flatMap(_.goodDescendants(accept))
    } else {
      Iterator.empty
    }
  }
}
