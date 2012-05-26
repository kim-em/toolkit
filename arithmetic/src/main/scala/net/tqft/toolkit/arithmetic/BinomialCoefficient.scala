package net.tqft.toolkit.arithmetic
//import net.tqft.toolkit.functions.Memo

object BinomialCoefficient {
  def apply(n: Int, k0: Int): Long = {
    var k = k0
    var t: Long = 1

    var m = n - k
    if (k < m) {
      k = m
    }

    for (i <- (k + 1 to n).reverse) {
      t = t * i / (n - i + 1)
    }
    t
  }

//  val cached = Memo(apply _)

}