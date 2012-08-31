package net.tqft.toolkit.algebra

import scala.collection.GenIterable

// TODO should have
//		@specialized(Int, Long, Float, Double) 
// but this crashes the compiler (somewhere in FiniteGroups??).
trait CommutativeSemigroup[@specialized(Int) A] {
  def add(x: A, y: A): A
  def add(x0: A, x1: A*): A = x1.fold(x0)(add _)
}

trait Zero[@specialized(Int, Long, Float, Double) A] {
  def zero: A
}

trait CommutativeMonoid[@specialized(Int, Long, Float, Double) A] extends CommutativeSemigroup[A] with Zero[A] {
  def add(xs: GenIterable[A]): A = xs.fold(zero)(add _)
}

trait Subtractive[@specialized(Int, Long, Float, Double) A] extends CommutativeSemigroup[A] {
  def negate(x: A): A
  def subtract(x: A, y: A) = add(x, negate(y))
}

trait CommutativeGroup[@specialized(Int, Long, Float, Double) A] extends CommutativeMonoid[A] with Subtractive[A]
