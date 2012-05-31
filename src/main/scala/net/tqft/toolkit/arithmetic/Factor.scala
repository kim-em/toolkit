package net.tqft.toolkit.arithmetic

object Factor {
  val cached: Int => List[Int] = net.tqft.toolkit.functions.Memo(apply _)
  def apply(n: Int): List[Int] = impl(n, Nil, Primes.view.takeWhile({ k => k * k <= n }).iterator)

  @scala.annotation.tailrec
  private def impl(n: Int, previousFactors: List[Int], primes: Iterator[Int]): List[Int] = {
    if (n < 0) {
      impl(-n, -1 :: previousFactors, primes)
    } else if (n == 0) {
      List(0)
    } else if (n == 1) {
      previousFactors.reverse
    } else if (!primes.hasNext) {
      (n :: previousFactors).reverse
    } else {
      val p = primes.next
      var m = n
      var k = 0
      while (m % p == 0) {
        k = k + 1
        m = m / p
      }
      impl(m, List.fill(k)(p) ::: previousFactors, primes)
    }
  }
}