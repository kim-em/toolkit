package net.tqft.toolkit.algebra.primes

import scala.io.Source
import net.tqft.toolkit.arithmetic.Mod._

object Admissible4 {
  val primes = Source.fromFile("/Users/scott/scratch/primes/primes.m").getLines.map(_.toInt).toArray
  def start(s: Array[Int]): Sequence = {
    val primeCutoff = primes.indexWhere(_ > /* s.size */ 55000 /* 5000 */ )
    val residueOccupancies = Array.tabulate(primeCutoff)({ i: Int =>
      if (i % 1000 == 0) println("starting up: " + i + "/" + primeCutoff)
      val p = primes(i)
      val o = Array.fill(p)(0)
      for (n <- s) o(n mod p) += 1
      o
    })

    Sequence(s, primeCutoff, residueOccupancies)
  }

  def pmin(x: Array[Int]): Int = {
    var m = x(0)
    for(n <- x) {
      if(n < m) m = n
      if(m == 0) return 0
    }
    m
  }
  
  case class Sequence(s: Array[Int], primeCutoff: Int, var residueOccupancies: Array[Array[Int]]) {
    val mins = residueOccupancies.map(_.min)
    var inadmissibility = mins.sum

    def worstResidue = {
      val i = mins.indexOf(mins.max)
      val j = residueOccupancies(i).indexOf(residueOccupancies(i).min)
      (i, j)
    }

    def width = s.max - min
    def min = s.min

    def mutate(probability: Int => Double): this.type = {
      val (wi, wj) = worstResidue

      val targets = (0 until s.size) filter (i => (s(i) mod primes(wi)) == wj)

      val ai = targets(scala.util.Random.nextInt(targets.size))
      val b = {
        val min = s.min
        val width = s.max - min
        Iterator.continually(scala.util.Random.nextInt(width + 1) + min).find(!s.contains(_)).get
      }

      val old = inadmissibility

      val a = s(ai)
      inadmissibility = 0
      for (i <- 0 until residueOccupancies.length) {
        residueOccupancies(i)(a mod primes(i)) -= 1
        residueOccupancies(i)(b mod primes(i)) += 1
        mins(i) = pmin(residueOccupancies(i))
        inadmissibility += mins(i)
      }
      s(ai) = b

      if (scala.util.Random.nextDouble < probability(inadmissibility - old)) {
        println("accepting")
        // accept
      } else {
        // back out
        println("rejecting")

        inadmissibility = old
        for (i <- 0 until residueOccupancies.length) {
          residueOccupancies(i)(b mod primes(i)) -= 1
          residueOccupancies(i)(a mod primes(i)) += 1
          mins(i) = pmin(residueOccupancies(i))
        }
        s(ai) = a
      }

      this
    }
  }
}

object Admissible4App extends App {
  //  println(Admissible4.start(Admissible.RichardsSequence(5553, 361640)).inadmissibility)
  //  val s = Admissible4.start(Admissible.RichardsSequence(355, 10000).toArray)
  val s = Admissible4.start(Admissible.RichardsSequence(5552, 341640).toArray)

  for (k <- Iterator.from(0)) {
    println("inadmissibility: " + s.inadmissibility + " and width: " + s.width)
    if (s.inadmissibility == 0) {
      println(Admissible(s.s))
      println("success after " + k + " steps")
      System.exit(0)
    }

    s.mutate({ d: Int => if (d < 0) 1.0 else scala.math.exp(-d * 2) })
  }

}