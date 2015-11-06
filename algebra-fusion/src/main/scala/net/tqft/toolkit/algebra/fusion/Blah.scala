package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import net.tqft.toolkit.algebra.enumeration.Odometer
import net.tqft.toolkit.Profiler

object Blah extends App {
  val r = 5

  val multiplicities = for (i <- 1 until r; j <- 1 until r; k <- 1 until r) yield Seq(i, j, k)
  val representativeMultiplicities = multiplicities.filter(m => m == m.sorted)
  val numberOfVariables=representativeMultiplicities.size
  val lookup = for (m <- multiplicities) yield representativeMultiplicities.indexOf(m.sorted)
  val lookupDiagonals = for (i <- 1 until r) yield representativeMultiplicities.indexOf(Seq(i, i, i))

  println(multiplicities)
  println(representativeMultiplicities)
  println(lookup)
  println(lookupDiagonals)

  def N(x: Array[Int])(i: Int, j: Int, k: Int) = {
    if (i == 0) {
      if (j == k) 1 else 0
    } else {
      if (j == 0) {
        if (i == k) 1 else 0
      } else {
        if (k == 0) {
          if (i == j) 1 else 0
        } else {
          x(lookup((i - 1) * (r - 1) * (r - 1) + (j - 1) * (r - 1) + (k - 1)))
        }
      }
    }
  }
  def R(x: Array[Int]) = {
    Array.tabulate(r, r)({ (i, j) =>
      var t = 0
      for (k <- 0 until r; l <- 0 until r) {
        t = t + N(x)(k, j, l) * N(x)(k, l, i)
      }
      t
    })
  }
  def associative_?(m: Array[Int]): Boolean = {
    for(x <- (0 until r).iterator; y<-0 until r; z<-0 until r;b<-0 until r) {
      var t = 0
      for(a <- (0 until r)) {
        t  = t + N(m)(x,y,a)*N(m)(a,z,b) - N(m)(x,a,b)*N(m)(y,z,a)
      }
      if(t != 0) return false
    }
    true
  }
  def globalDimension(x: Array[Int]) = {
    FrobeniusPerronEigenvalues.estimateWithEigenvector(R(x), 0.1, globalDimensionBound + 1)._1 
  }

  val globalDimensionBound = 41.0

  def limit(x: Array[Int]) = {
    (for (Seq(a, b) <- lookupDiagonals.sliding(2)) yield {
      x(b) <= x(a)
    }).forall(_ == true) &&
      globalDimension(x) <= globalDimensionBound
  }

  val prefixLength = 5
  val prefixes = {
    val max = math.floor(math.sqrt(globalDimensionBound)).toInt
    val range = 0 to max
    Seq.fill(prefixLength)(range).foldLeft(Seq(Seq[Int]()))({ (s, r) => for(s0 <- s; r0 <- r) yield r0 +: s0 }).map(_.toArray)
  }
  
  val chunks = for(prefix <- prefixes.par; xs = Odometer.apply({x: Array[Int] => limit(x ++ prefix)} )(Array.fill(numberOfVariables-prefixLength)(0)); x <- xs; if associative_?(x ++ prefix)) yield x ++ prefix
  println(chunks.size)
  
//  def xs = Odometer.apply(limit)(Array.fill(numberOfVariables)(0)).filter(associative_?)
//  
//  println(xs.size)
//  println((chunks.seq.toSet.map({x: Array[Int] => x.toList}) -- xs.map(_.toList)).size)
//  
////  for(x <- (chunks.seq.toSet -- xs)) {
////    println(x.toList)
////    println(globalDimension(x))
////  }
//  println(Profiler.timing(xs.size))
}