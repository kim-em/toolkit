package net.tqft.toolkit.algebra.khovanov

import net.tqft.toolkit.algebra.AdditiveSemigroup

trait Category[O, M] {
  def source(x: M): O
  def target(x: M): O
  def identity(o: O): M
  def compose(x: M, y: M): M
}
trait CategoryWithZero[O, M] extends Category[O, M] {
  def zero(source: O, target: O): M
}
trait AdditiveCategory[O, M] extends CategoryWithZero[O, M] with AdditiveSemigroup[M]
trait TensorCategory[O, M] extends AdditiveCategory[O, M] {
  def tensorObjects(x: O, y: O): O
  def tensorMorphisms(x: M, y: M): M
}

trait Functor[O1, M1, O2, M2] {
  def applyToObject(o: O1): O2
  def applyToMorphism(m: M1): M2
}

