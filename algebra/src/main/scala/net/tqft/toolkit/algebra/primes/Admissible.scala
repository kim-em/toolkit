package net.tqft.toolkit.algebra.primes

import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.util.Date

import scala.collection.mutable


object Admissible {
  val k0 = 3500000
  val k1 = 2947442
  val k2 = 2618607
  val k3 = 866805
  val k4 = 341640
  val m0 = 250150
  val m3 = 69000
  val m4 = 30000

  val primes = Source.fromFile("/Users/scott/scratch/primes/primes.m").getLines.map(_.toInt).toArray
  val primesView = primes.view
  require(primes(k0 + m0 - 1) == 63374611)
  println("Finished loading primes...")

  var lastFailure = 2

  def apply(set: Iterable[Int]): Boolean = {
    var counter = m4

    var result = true

    def check(p: Int) = {
      result && (if (apply(set, p)) true else { lastFailure = p; result = false; false })
    }

    val chunks = Seq(Seq(lastFailure), primesView.take(m4).filter(_ > lastFailure).take(1000), primesView.take(m4).filter(_ > lastFailure).drop(1000), primesView.take(m4).filter(_ < lastFailure))

    chunks.forall({ chunk =>
      chunk.par.forall({ p =>
        counter -= 1
        if (counter % 1000 == 0) println(counter + " steps remaining; considering p=" + p)
        check(p)
      })
    })

  }

  def apply(set: Iterable[Int], p: Int): Boolean = {
    val bs = new scala.collection.mutable.BitSet(p)
    import net.tqft.toolkit.arithmetic.Mod._
    set.foreach(t => bs += t mod p)
    val result = bs.size < p
    if (!result) {
      println("failure witnessed by p=" + p)
    }
    result
  }

  def check(m: Int): Boolean = {
    apply(primes.drop(m).take(k3))
  }

  def check2(m: Int): Boolean = {
    val rs = RichardsSequence(m, k4)
    val r = apply(rs)
    if(r) println("width = " + (rs.max - rs.min))
    r
  }
  
  def RichardsSequence(m: Int, k: Int) = {
    val h0 = primesView.drop(m).take((k + 1) / 2 - 1)
    val h1 = primesView.drop(m).take(k / 2 - 1).map(-_)
    val h2 = Seq(1, -1).view
    h0 ++ h1 ++ h2
  }
}

object AdmissibleApp extends App {
  //	33639 is the first that works for check with k_0
  // 36716 is the first that works for check2 with k_0

  // 30798 is the first that works for check2 with k_1

  Admissible.lastFailure = 2
  println((1 to Admissible.m0).find({ m =>
    println(new Date() + " trying m=" + m)
    Admissible.check2(m)
  }))

}
