package net.tqft.toolkit.algebra

trait Semigroup[A] {
  def multiply(x: A, y: A): A
  def multiply(x0: A, x1: A*): A = x1.fold(x0)(multiply _)
  def power(x: A, k: Int): A = {
    // TODO no need for this to be recursive; just use the binary expansion of k.
    require(k >= 1)
    if (k == 1) {
      x
    } else if (k % 2 == 1) {
      multiply(x, power(multiply(x, x), k / 2))
    } else {
      power(multiply(x, x), k / 2)
    }
  }
}
