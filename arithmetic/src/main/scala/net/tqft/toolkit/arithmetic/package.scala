package net.tqft.toolkit

package object arithmetic {
  def ??? = throw new NoSuchMethodException

  // from http://stackoverflow.com/a/6824828/82970
  val Primes = {
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

}