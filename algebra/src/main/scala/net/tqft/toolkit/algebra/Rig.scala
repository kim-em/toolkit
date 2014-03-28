package net.tqft.toolkit.algebra

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