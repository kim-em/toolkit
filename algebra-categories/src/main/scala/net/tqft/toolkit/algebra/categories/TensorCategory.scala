package net.tqft.toolkit.algebra.categories

trait TensorCategory[O, M, R] extends LinearCategory[O, M, R] {
  def tensorObjects(o1: O, o2: O): O
  def tensorMorphisms(m1: M, m2: M): M
}
