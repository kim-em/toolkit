package net.tqft.toolkit.algebra

import scala.collection.GenSeq

trait One[A] {
  def one: A
}

trait Monoid[A] extends Semigroup[A] with One[A] {
  def multiply(xs: GenSeq[A]): A = xs.fold(one)(multiply _)
  override def power(x: A, k: Int): A = {
    if (k == 0) {
      one
    } else {
      super.power(x, k)
    }
  }
  def orderOfElement(a: A): Int = Iterator.iterate(a)(multiply(_, a)).indexOf(one) + 1
}
