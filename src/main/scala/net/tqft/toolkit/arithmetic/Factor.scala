package net.tqft.toolkit.arithmetic

object Factor {

  // from http://stackoverflow.com/a/6824828/82970
  val primes = {
    case class Cross(next: Int, incr: Int)

    def adjustCrosses(crosses: List[Cross], current: Int) = {
      def nextIncr = crosses collect {
        case cross @ Cross(`current`, incr) => cross copy (next = current + incr)
      }

      def unchangedCrosses = crosses filter (_.next != current)

      nextIncr ::: unchangedCrosses
    }

    def notPrime(crosses: List[Cross], current: Int) = crosses exists (_.next == current)

    def sieve(s: Stream[Int], crosses: List[Cross]): Stream[Int] = {
      val current #:: rest = s

      if (notPrime(crosses, current)) sieve(rest, adjustCrosses(crosses, current))
      else current #:: sieve(rest, Cross(current * current, current) :: crosses)
    }
    sieve(Stream from 2, Nil)
  }

  def apply(n: Int): List[Int] = apply(n, Nil, primes takeWhile ({ k => k * k <= n }) iterator)

  @scala.annotation.tailrec
  private def apply(n: Int, previousFactors: List[Int], primes: Iterator[Int]): List[Int] = {
    if(n < 0) {
      apply(-n, -1 :: previousFactors, primes)
    } else if (n == 1) {
      previousFactors.reverse
    } else if(!primes.hasNext) {
      (n :: previousFactors).reverse
    } else {
      val p = primes.next
      var m = n
      var k = 0
      while (m % p == 0) {
        k = k + 1
        m = m / p
      }
      apply(m, List.fill(k)(p) ::: previousFactors, primes)
    }
  }
}