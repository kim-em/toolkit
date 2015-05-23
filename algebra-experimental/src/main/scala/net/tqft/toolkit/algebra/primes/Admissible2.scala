package net.tqft.toolkit.algebra.primes

import scala.io.Source
import java.util.Date

import scala.collection.mutable
import net.tqft.toolkit.arithmetic.Mod._


trait PerhapsAdmissible {
  def sequence: mutable.Set[Int]
  def admissible_? : Boolean
  def replace(a: Int, b: Int): this.type
}

object Admissible2App extends App {
  println(Admissible2(Seq(3, 5)).admissible_?)
  println(Admissible2.findOffset(35, Admissible2.primes))
}

object Admissible2 {
  val primes = Source.fromFile("/Users/scott/scratch/primes/primes.m").getLines.map(_.toInt).toVector

  def apply(sequence: Iterable[Int]): PerhapsAdmissible = {
    val numberOfPrimes = primes.indexWhere(_ > sequence.size)

    PerhapsAdmissibleImpl(mutable.Set() ++ sequence,
      mutable.IndexedSeq.fill(numberOfPrimes)(None),
      0,
      {
        val p = sequence.partition(p => (p mod 2) == 0)
        IndexedSeq(mutable.Set() ++ p._1, mutable.Set() ++ p._2)
      })
  }

  def findOffset(k: Int, longSequence: IndexedSeq[Int], always: Seq[Int] = Nil) = {
    var m = 0
    val sequence = apply(longSequence.take(k - always.size) ++ always)
    while (!sequence.admissible_?) {
      sequence.replace(longSequence(m), longSequence(m + k))
      m += 1
    }
    m
  }

  private case class PerhapsAdmissibleImpl(
    sequence: mutable.Set[Int],
    primeVacancies: mutable.IndexedSeq[Option[(mutable.BitSet, Int)]], // these are intended as overestimates; more 1s marked than are really there
    var lastOccupiedPrime: Int,
    var residueClasses: IndexedSeq[mutable.Set[Int]]) extends PerhapsAdmissible {

    var _admissible_? = false

    override def replace(a: Int, b: Int) = {
      println("replacing " + a + " with " + b + ", p=" + primes(lastOccupiedPrime))
      sequence -= a += b
      for ((Some((bs, c)), i) <- primeVacancies.view.zipWithIndex) {
        if (bs.add(b mod primes(i))) {
          primeVacancies(i) = Some((bs, c + 1))
        }
      }
      residueClasses(a mod primes(lastOccupiedPrime)) -= a
      residueClasses(b mod primes(lastOccupiedPrime)) += b

      admissible_?

      this
    }

    override def admissible_? = {
      updateOccupiedPrime
      _admissible_?
    }

    def updateOccupiedPrime: Unit = {
      if (residueClasses.exists(_.isEmpty)) {
        // time to find a new prime
        val view = primeVacancies.view.zipWithIndex
        val candidates = view.filter({
          case p =>
            p._1.nonEmpty && p._1.get._2 == primes(p._2)
        }) ++ view.filter({ p => p._1.isEmpty })

        candidates.find({
          case (obs, i) =>
            val p = Admissible2.primes(i)
            val bs = new scala.collection.mutable.BitSet(p)
            var c = 0
            sequence.foreach(t => if (bs.add(t mod p)) c += 1)
            primeVacancies(i) = Some((bs, c))
            p == c
        }) match {
          case None => {
            // we win, mark as admissible!
            _admissible_? = true
          }
          case Some((_, i)) => {
            val p = primes(i)
            println("switching to a new prime: " + p)
            lastOccupiedPrime = i
            residueClasses = IndexedSeq.fill(p)(mutable.Set())
            for (s <- sequence) {
              residueClasses(s mod p) += s
            }
          }
        }
      }
    }
  }

}
