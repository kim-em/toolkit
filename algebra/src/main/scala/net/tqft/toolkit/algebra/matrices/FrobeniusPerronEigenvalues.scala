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
    //require(m == m.transpose)

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
}