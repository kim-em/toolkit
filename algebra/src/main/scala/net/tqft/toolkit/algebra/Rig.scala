package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.polynomials.Polynomial

trait Rig[@specialized(Int, Long, Float, Double) A] extends Monoid[A] with AdditiveMonoid[A] {
  def multiplyByInt(x: A, y: Int): A = multiply(x, fromInt(y))
  def fromInt(x: Int): A
}

object Rig {
  implicit val CountableCardinals: Rig[CountableCardinal] = CountableCardinals
  implicit def forget[A:Ring]: Rig[A] = implicitly[Ring[A]]  
}