package net.tqft.toolkit.algebra

import scala.collection.GenSeq

trait CommutativeSemigroup[A] {
  def add(x: A, y: A): A
  def add(x0: A, x1: A*): A = x1.fold(x0)(add _)
}

trait Zero[A] {
  def zero: A
}

trait CommutativeMonoid[A] extends CommutativeSemigroup[A] with Zero[A] {
  def add(xs: GenSeq[A]): A = xs.fold(zero)(add _)
}

trait Subtractive[A] extends CommutativeSemigroup[A] {
  def negate(x: A): A
  def subtract(x: A, y: A) = add(x, negate(y))
}

trait CommutativeGroup[A] extends CommutativeMonoid[A] with Subtractive[A]
