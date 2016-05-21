package net.tqft.toolkit.algebra.fusion4

import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import net.tqft.toolkit.algebra.enumeration.Odometer

object FirstMatrices {
  def apply(rank: Int, maximumDimension: Double): Iterator[Array[Array[Int]]] = {

    var eigenvector: Array[Double] = Array.fill(rank)(1.0)
    def limit(m: Array[Array[Int]]) = {
      val (eigenvalue, nextEigenvector) = FrobeniusPerronEigenvalues.estimateWithEigenvector(m, 0.001, maximumDimension, Some(eigenvector))
      eigenvector = nextEigenvector
      eigenvalue < maximumDimension
    }

    val empty = {
      val n = Array.fill(rank, rank)(0)
      n(0)(1) = 1
      n(1)(0) = 1
      n
    }

    implicit val odometer: Odometer[Array[Array[Int]]] = new Odometer[Array[Array[Int]]] {
      override def reset(m: Array[Array[Int]]) = {
        empty
      }
      override def carry(m: Array[Array[Int]]): Option[Array[Array[Int]]] = {
        var i = 1
        var j = 1
        while (m(i)(j) == 0) {
          j = j + 1
          if (j == rank) {
            i = i + 1
            j = i
            if (i == rank) return None
          }
        }
        var j1 = j + 1
        var i1 = if (j1 == rank) {
          j1 = i + 1
          i + 1
        } else {
          i
        }
        if (i1 == rank) return None
        val r = m.clone
        r(i) = r(i).clone
        r(j) = r(j).clone
        r(i1) = r(i1).clone
        r(j1) = r(j1).clone
        r(i)(j) = 0
        r(j)(i) = 0
        r(i1)(j1) = r(i1)(j1) + 1
        r(j1)(i1) = r(j1)(i1) + 1
        Some(r)
      }
      override def increment(m: Array[Array[Int]]): Array[Array[Int]] = {
        val n = m.clone
        n(1) = n(1).clone
        n(1)(1) = n(1)(1) + 1
        n
      }
    }

    val matrices = Odometer(limit)(empty)
    
    matrices.filter(_.forall(_.exists(_ != 0))) // every row is nonzero
      .filter(_(1).tail.exists(_ != 0)) // the object isn't invertible
  }
}