package net.tqft.toolkit.algebra

// TODO should have
//		@specialized(Int, Long, Float, Double) 
// but this crashes the compiler.
trait Semigroup[@specialized(Int, Long, Float, Double) A] {
  def multiply(x: A, y: A): A
  def multiply(x0: A, x1: A, x2: A*): A = {
    x2.fold(multiply(x0, x1))(multiply)
  }
  def power(x: A, k: Int): A = {
    // modified from https://github.com/dlwh/breeze/blob/master/math/src/main/scala/breeze/numerics/IntMath.scala
    // under http://www.apache.org/licenses/LICENSE-2.0
    require(k >= 1)
    if (k == 1) {
      x
    } else {
      var b = x
      var e = k
      var result: Option[A] = None
      while (e != 0) {
        if ((e & 1) != 0) {
          result = result.map(multiply(_, b)).orElse(Some(b))
        }
        e >>= 1
        b = multiply(b, b)
      }

      result.get
    }
  }
}

object Semigroup {
  implicit def forget[A: Monoid]: Semigroup[A] = implicitly[Semigroup[A]]
}
