package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.polynomials.Polynomial

trait Rig[@specialized(Int, Long, Float, Double) A] extends Monoid[A] with AdditiveMonoid[A] {
  def multiplyByInt(x: A, y: Int): A = multiply(x, fromInt(y))
  def fromInt(x: Int): A
}

object Rig {
  implicit val CountableCardinals: Rig[CountableCardinal] = CountableCardinals
  implicit def forget[A: Ring]: Rig[A] = implicitly[Ring[A]]
}

object Rigs {
  def adjoinUnknown[A: Rig]: Rig[Option[A]] = {
    val rig = implicitly[Rig[A]]
    new Rig[Option[A]] {
      override def zero = Some(rig.zero)
      override def one = Some(rig.one)
      override def fromInt(k: Int) = Some(rig.fromInt(k))
      override def add(x: Option[A], y: Option[A]) = for (xa <- x; ya <- y) yield rig.add(xa, ya)
      override def multiply(x: Option[A], y: Option[A]) = ???
    }
  }
}