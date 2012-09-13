package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.polynomials.Polynomial

trait Rig[@specialized(Int, Long, Float, Double) A] extends Monoid[A] with AdditiveMonoid[A] {
  def multiplyByInt(x: A, y: Int): A = multiply(x, fromInt(y))
  def fromInt(x: Int): A = {
    require(x >= 0)
    if(x == 0) {
      zero
    } else {
      Iterator.fill(x)(one).reduce(add)
    }
  }
}

object Rig {
  implicit def forget[A: Ring]: Rig[A] = implicitly[Ring[A]]
}

trait CommutativeRig[A] extends Rig[A]


object Unknowns {
  sealed trait ?
  case object ? extends ?
}

object Rigs {
  import Unknowns.?
  import net.tqft.toolkit.UnionTypes._
  
  def adjoinUnknown[A: Rig]: Rig[A or ?] = {
    val rig = implicitly[Rig[A]]
    new Rig[A or ?] {
      override def zero = rig.zero
      override def one = rig.one
      override def fromInt(k: Int) = rig.fromInt(k)
      override def add(x: A or ?, y: A or ?) = for (xa <- x.left; ya <- y.left) yield rig.add(xa, ya)
      override def multiply(x: A or ?, y: A or ?) = {
        if(x == Left(rig.zero) || y == Left(rig.zero)) {
          rig.zero
        } else {
          for (xa <- x.left; ya <- y.left) yield rig.multiply(xa, ya)
        }
      }
    }
  }
}