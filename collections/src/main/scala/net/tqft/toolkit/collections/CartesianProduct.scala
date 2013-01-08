package net.tqft.toolkit.collections

object CartesianProduct {
  implicit def toProductable[A](sets: Seq[Traversable[A]]) = new productable(sets)
  
  class productable[A](sets: Seq[Traversable[A]]) {
    def cartesianProduct = sets.reverse.foldLeft(Iterator[Seq[A]](Nil))((x, y) => for (a <- x; b <- y) yield b +: a)
  }
}