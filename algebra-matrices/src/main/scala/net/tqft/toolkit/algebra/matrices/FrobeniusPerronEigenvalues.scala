package net.tqft.toolkit.algebra.matrices

import net.tqft.toolkit.functions.FixedPoint
import com.google.common.cache.CacheLoader
import com.google.common.cache.CacheBuilder
import java.util.concurrent.TimeUnit

object FrobeniusPerronEigenvalues {

  def estimate(m: Array[Array[Int]]): Double = estimateWithEigenvector(m)._1
  
  def estimateWithEigenvector(m: Array[Array[Int]]): (Double, Seq[Double]) = {
    val rank = m.length

    var iv0 = m.map(_.sum + 1.0)
    var est1 = scala.math.sqrt(iv0.map(x => x * x).sum)
    for (i <- 0 until rank) {
      iv0(i) = iv0(i) / est1
    }
    var iv1 = Array.fill(rank)(0.0)
    var est0 = 0.0
    var k = 0

    while (scala.math.abs(est1 - est0) > 0.0001) {
      est0 = est1
      est1 = 0.0
      k = 0
      while (k < rank) {
        var j = 0
        iv1(k) = 0.0
        while (j < rank) {
          iv1(k) += m(k)(j) * iv0(j)
          j += 1
        }
        est1 += iv1(k) * iv1(k)
        k += 1
      }
      est1 = scala.math.sqrt(est1)
      k = 0
      while (k < rank) {
        iv0(k) = iv1(k) / est1
        k += 1
      }
    }

    (est1, iv0)
  }
}