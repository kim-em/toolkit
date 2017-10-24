package net.tqft.toolkit.algebra.matrices

import net.tqft.toolkit.functions.FixedPoint
import com.google.common.cache.CacheLoader
import com.google.common.cache.CacheBuilder
import java.util.concurrent.TimeUnit
import scala.util.Random
import net.tqft.toolkit.Logging

object FrobeniusPerronEigenvalues {

  // really should do the decomposition into irreducibles, as in:
  // Validated computation tool for the Perron-Frobenius eigenvalues, Kaori Nagatou and Yutaka Ishii
  
  def estimate(m: Array[Array[Int]]): Double = estimateWithEigenvector(m)._1

  def estimateWithEigenvector(m: Array[Array[Int]], tolerance: Double = 0.0001, target: Double = Double.MaxValue, hint: Option[Array[Double]] = None): (Double, Array[Double]) = {
    val rank = m.length

//        for(i <- 0 until rank; j <- 0 until rank) require(m(i)(j) == m(j)(i))
        
    val max = m.map(_.max).max

    var iv0 = hint.map(_.clone).getOrElse(m.map(_.sum + 1.0))

    if (max > target) {
      (max, iv0)
    } else {

      var est1 = scala.math.sqrt(iv0.map(x => x * x).sum)
      for (i <- 0 until rank) {
        iv0(i) = iv0(i) / est1
      }
      var iv1 = Array.fill(rank)(0.0)
      var est0 = 0.0
      var k = 0

      while (est0 == 0.0 || est1 < target && scala.math.abs(est1 - est0) > tolerance) {
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

      if (est1 < target && est1 < max - 1) {
//        Logging.warn("Restarting eigenvalue estimate, it looks like we got stuck.")
        estimateWithEigenvector(m, tolerance, target, Some(iv0.map(_ + Random.nextDouble)))
      } else {
        (est1, iv0)
      }
    }
  }
  
  def upperBound(m: Array[Array[Int]], approximateEigenvector: Array[Double]): Double = {
    val vector = approximateEigenvector.clone.map({  x => if(x < 0.0001) 0.0001 else x })
    val ratios = Array.tabulate(m.length)({ i => 
      Array.tabulate(vector.length)({ j => 
        m(i)(j) * vector(j)  
      }).sum / vector(i)
    })
    ratios.max
  }
}