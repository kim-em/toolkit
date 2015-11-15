package net.tqft.toolkit.algebra.khovanov

import net.tqft.toolkit.algebra.AdditiveSemigroup

trait Category[O, M] {
  def source(x: M): O
  def target(x: M): O
  def identity(o: O): M
  def compose(x: M, y: M): M
}
object Category {
  implicit def forget[O, M](implicit category: TensorCategory[O, M]): Category[O, M] = category
}
trait CategoryWithZero[O, M] extends Category[O, M] {
  def zero(source: O, target: O): M
}
trait AdditiveCategory[O, M] extends CategoryWithZero[O, M] with AdditiveSemigroup[M]
object AdditiveCategory {
  implicit def forget[O, M](implicit category: AdditiveTensorCategory[O, M]): AdditiveCategory[O, M] = category
}
trait TensorCategory[O, M] extends Category[O, M] {
  def tensorObjects(x: O, y: O): O
  def tensorMorphisms(x: M, y: M): M
}
object TensorCategory {
  implicit def forget[O, M](implicit category: AdditiveTensorCategory[O, M]): TensorCategory[O, M] = category
}
trait AdditiveTensorCategory[O, M] extends TensorCategory[O, M] with AdditiveCategory[O, M]

trait Functor[O1, M1, O2, M2] {
  def applyToObject(o: O1): O2
  def applyToMorphism(m: M1): M2
}

trait Factorisation[O, M] { this: Category[O, M] =>
  def findLeftFactorisationThrough(a: M)(b: M): Option[M] // Some(c) where b = a c
  def findRightFactorisationThrough(a: M)(b: M): Option[M] // Some(c) where b = c a

  def findFactorisationsThrough(a: M)(left: Seq[M], right: Seq[M]): Option[(Seq[M], Seq[M])] = {
    // if we can factor a out of everything, do so!
    ???
  }
}