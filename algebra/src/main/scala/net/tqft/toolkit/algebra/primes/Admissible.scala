package net.tqft.toolkit.algebra.primes

import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.util.Date

object Admissible {
  val k0 = 3500000
  val k1 = 2947442
  val k2 = 2618607
  val m0 = 250150

  val primes = Source.fromFile("/Users/scott/scratch/primes/primes.m").getLines.map(_.toInt).toVector
  require(primes(k0 + m0 - 1) == 63374611)
  println("Finished loading primes...")

  var lastFailure = 2

  def apply(set: IndexedSeq[Int]): Boolean = {
    var counter = m0

    var result = true

    def check(p: Int) = {
      result && (if (apply(set, p)) true else { lastFailure = p; result = false; false })
    }

    val chunks = Seq(Seq(lastFailure), primes.take(m0).filter(_ > lastFailure).take(1000), primes.take(m0).filter(_ > lastFailure).drop(1000), primes.take(m0).filter(_ < lastFailure))
    
    chunks.forall({ chunk => chunk.par.forall({ p =>
      counter -= 1
      if (counter % 1000 == 0) println(counter + " steps remaining; considering p=" + p)
      check(p)
    })})
        

  }

  def apply(set: IndexedSeq[Int], p: Int): Boolean = {
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
    apply(primes.drop(m).take(k2))
  }

  def check2(m: Int): Boolean = {
    val h0 = 1 +: primes.drop(m).take((k2+1) / 2 - 1)
    apply(h0 ++ h0.map(-_))
  }
}

object AdmissibleApp extends App {
  //	33639 is the first that works for check with k_0
  // 36716 is the first that works for check2 with k_0

  // 30798 is the first that works for check2 with k_1
  
  Admissible.lastFailure = 2
  println((25000 to Admissible.m0).find({ m =>
    println(new Date() + " trying m=" + m)
    Admissible.check2(m)
  }))

  
}
