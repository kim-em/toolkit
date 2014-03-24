package net.tqft.toolkit.algebra

trait Enumerable[A] {
  def elements: Iterable[A]
}

trait Finite[A] extends Enumerable[A] {
  override def elements: Set[A]
  def size = elements.size
}

