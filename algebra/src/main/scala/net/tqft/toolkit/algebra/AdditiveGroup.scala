package net.tqft.toolkit.algebra

trait Subtractive[@specialized(Int, Long, Float, Double) A] extends AdditiveSemigroup[A] {
  def negate(x: A): A
  def subtract(x: A, y: A) = add(x, negate(y))
}

trait AdditiveGroup[@specialized(Int, Long, Float, Double) A] extends AdditiveMonoid[A] with Subtractive[A]

object AdditiveGroup {
  implicit def forget[A:Ring]: AdditiveGroup[A] = implicitly[AdditiveGroup[A]]  
}