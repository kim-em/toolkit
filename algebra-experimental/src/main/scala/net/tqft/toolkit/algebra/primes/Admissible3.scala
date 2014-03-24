package net.tqft.toolkit.algebra.primes

import scala.collection.mutable
import net.tqft.toolkit.arithmetic.Mod._
import scala.io.Source

object Admissible3App extends App {
  val s = Admissible3.greedy().toStream

//  println(s.take(20).toList)
//
//    for(n <- Iterator.from(0, 1000)) {
//      println(n -> s(n))
//    }
  
  for(i <- Iterator.from (0)) {
    val x = Admissible3.greedy().toStream(10000) // 108012
    if(x < 108260) println(x)
  }
}

case class SizedBitSet(bs: mutable.BitSet) {
  var size = bs.size
  def remove(i: Int) {
    if(bs.remove(i)) {
      size -= 1
    }
  }
  def head = bs.head
}

object Admissible3 {
  val primes = Source.fromFile("/Users/scott/scratch/primes/primes.m").getLines.map(_.toInt).toArray

  def greedy(): Iterator[Int] = {
    val blockedPrimes = mutable.ListBuffer[(Int, Int)]() // primes with only one remaining residue class
    val residues = mutable.ListBuffer[(Int, SizedBitSet)]() // "active" primes, with the occupancy of the residue classes, and the number of remaining residue classes

    residues += ((2, SizedBitSet(mutable.BitSet(0,1))))

    var size = 0

    var nextPrimeIndex = 1

    // TODO should construct a Stream, since we're keeping all the old numbers around anyway!
    val previous = mutable.ListBuffer[Int]()

    def accept(n: Int): Boolean = {
//      println("considering n=" + n)

      val result = blockedPrimes.forall({
        case (p, i) => (n mod p) != i
      })

      if (result) {
//        println("accepted!")

        size += 1
        previous += n
        val p = primes(nextPrimeIndex)
        if (size == p - 2) { // start tracking new primes before it can be almost-filled
          val bs = new mutable.BitSet(p)
          for (i <- 0 until p) bs += i // fill up bs
          for (m <- previous) bs -= (m mod p)
          require(bs.size > 1)
          residues += ((p, SizedBitSet(bs)))
          nextPrimeIndex += 1
        }

        for ((p, bs) <- residues) {
          bs.remove(n mod p)
          if(bs.size == 1) {
              residues -= ((p, bs))
              blockedPrimes += ((p, bs.head))            
          }          
        }

//        println(residues)
//        println(blockedPrimes)
      } else {
//        println("rejected!")
      }

      result
    }

    Iterator.from(0).filter(accept)
  }
}