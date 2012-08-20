package net.tqft.toolkit.algebra

import net.tqft.toolkit.functions.FixedPoint

object FrobeniusPerronEigenvalues {
  def estimate[I: IntegerModel, R: ApproximateReals](m: Matrix[I]): R = {
    val integers = implicitly[IntegerModel[I]]
    val reals = implicitly[ApproximateReals[R]]

    val mR = m.mapEntries[R](reals.fromInteger)
    val initialVector = m.entries.map(v => reals.abs(reals.fromInteger(integers.add(v)))).seq
    val initialEstimate = reals.zero

    def next(p: (Seq[R], R)) = {
      val w = mR(p._1)
      (w.map(x => reals.quotient(x, w(0))), w(0))
    }

    println(m)
    
    FixedPoint.withSameTest({ (p: (Seq[R], R), q: (Seq[R], R)) => reals.close(p._2, q._2) })(next)(initialVector, initialEstimate)._2
  }
}