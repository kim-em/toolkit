package net.tqft.toolkit.algebra.fusion2

object AssociativityConstraints {

  def apply[V](rank: Int, f: (Int, Int, Int) => V): Seq[Quadratic[V]] = {
    (for (i <- (0 until rank).iterator; j <- (0 until rank).iterator; k <- (0 until rank).iterator; l <- (0 until rank).iterator) yield {
      val leftTerms = for (m <- 0 until rank) yield {
        QuadraticTerm(1, LinearTerm(0, Map(f(i, j, m) -> 1)), LinearTerm(0, Map(f(m, k, l) -> 1)))
      }
      val rightTerms = for (m <- 0 until rank) yield {
        QuadraticTerm(-1, LinearTerm(0, Map(f(i, m, l) -> 1)), LinearTerm(0, Map(f(j, k, m) -> 1)))
      }
      Quadratic(LinearTerm[V](0, Map.empty), leftTerms ++ rightTerms)
    }).toStream
  }

}