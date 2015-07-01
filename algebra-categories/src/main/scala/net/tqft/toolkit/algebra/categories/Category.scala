package net.tqft.toolkit.algebra.categories

trait Category[O, M] {
  def identityMorphism(o: O): M
  def source(m: M): O
  def target(m: M): O
  def compose(x: M, y: M): M
}
