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
}