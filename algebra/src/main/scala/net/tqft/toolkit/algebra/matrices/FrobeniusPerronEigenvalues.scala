package net.tqft.toolkit.algebra.matrices

import net.tqft.toolkit.algebra.ApproximateReals
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.functions.FixedPoint

object FrobeniusPerronEigenvalues {
  def estimate[I: IntegerModel, R: ApproximateReals](m: Matrix[I]): R = {
    val integers = implicitly[IntegerModel[I]]
    val reals = implicitly[ApproximateReals[R]]

    val mR = m.mapEntries[R](reals.fromInteger)
    val initialVector = reals.normalize(m.entries.map(v => reals.abs(reals.fromInteger(integers.add(integers.sum(v), integers.one)))).seq)
    val initialEstimate = reals.zero

    def next(p: (Seq[R], R)) = {
      val w = mR(p._1)
      val n = reals.norm(w)
      val nw = w.map(x => reals.quotient(x, n))
      (nw, n)
    }

    FixedPoint.withSameTest({ (p: (Seq[R], R), q: (Seq[R], R)) => reals.close(p._2, q._2) })(next)(initialVector, initialEstimate)._2
  }

  private val matrices = Matrices.over[Double]

  def estimate2(m: Matrix[Int]): Double = {
    //    require(m == m.transpose)

    val reals = implicitly[ApproximateReals[Double]]

    val mR = m.mapEntries(_.toDouble)
    val mR2 = matrices.compose(mR, mR)
    val iv = mR2.entries.map(v => v.sum + 1).seq
    val initialVector = reals.normalize(iv)
    val initialEstimate = reals.zero

    def next(p: (Seq[Double], Double)) = {
      val w = mR2(p._1)
      val n = reals.norm(w)
      val nw = w.map(x => reals.quotient(x, n))
      (nw, n)
    }

    scala.math.sqrt(FixedPoint.withSameTest({ (p: (Seq[Double], Double), q: (Seq[Double], Double)) => (p._2 - q._2).abs < 0.0001 })(next)(initialVector, initialEstimate)._2)
  }

//  def estimateWithEigenvector(m: Matrix[Int], hint: Option[Seq[Double]] = None): (Double, Seq[Double]) = estimateWithEigenvector(m.entries.map(_.toArray).toArray, hint)
  def estimateWithEigenvector(m: Array[Array[Int]], hint: Option[Seq[Double]] = None): (Double, Seq[Double]) = {
    val mR = m.map(_.map(_.toDouble))
    val rank = mR.length
    var iv0 = hint.map(_.toArray).getOrElse(mR.map(_.sum + 1))
    var est1 = scala.math.sqrt(iv0.map(x => x * x).sum)
    for(i <- 0 until rank) {
      iv0(i) = iv0(i) / est1
    }
    var iv1 = Array.fill(rank)(0.0)
    var est0 = 0.0
    var k = 0
    var count = 0
    while (scala.math.abs(est1 - est0) > 0.0001) {
      est0 = est1
      est1 = 0.0
      k = 0
      while (k < rank) {
        var j = 0
        iv1(k) = 0.0
        while (j < rank) {
          iv1(k) += mR(k)(j) * iv0(j)
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