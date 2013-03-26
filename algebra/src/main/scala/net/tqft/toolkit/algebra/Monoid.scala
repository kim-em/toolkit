package net.tqft.toolkit.algebra

import scala.collection.GenTraversableOnce

trait One[@specialized(Int, Long, Float, Double) A] {
  def one: A
}

trait Monoid[@specialized(Int, Long, Float, Double) A] extends Semigroup[A] with One[A] {
  def product(xs: GenTraversableOnce[A]): A = xs.reduceOption(multiply _).getOrElse(one)
  override def power(x: A, k: Int): A = {
    // modified from https://github.com/dlwh/breeze/blob/master/math/src/main/scala/breeze/numerics/IntMath.scala
    // under http://www.apache.org/licenses/LICENSE-2.0
    require(k >= 0)
    if (k == 0) {
      one
    } else if (k == 1) {
      x
    } else {
      var b = x
      var e = k
      var result = one
      while (e != 0) {
        if ((e & 1) != 0) {
          result = multiply(result, b)
        }
        e >>= 1
        b = multiply(b, b)
      }

      result
    }
  }
  def orderOfElement(a: A): Int = Iterator.iterate(a)(multiply(_, a)).indexOf(one) + 1
}
