package net.tqft.toolkit.algebra.fusion

object AssociativityConstraints {

  def apply[V](rank: Int, f: (Int, Int, Int) => V): Seq[((Int,Int,Int,Int),Quadratic[V])] = {
    import net.tqft.toolkit.collections.SmallMaps._
    
    (for (i <- (0 until rank).iterator; j <- (0 until rank).iterator; k <- (0 until rank).iterator; l <- (0 until rank).iterator) yield {
      val leftTerms = for (m <- 0 until rank) yield {
        QuadraticTerm(1, LinearTerm(0, new Map1(f(i, j, m), 1)), LinearTerm(0, new Map1(f(m, k, l), 1)))
      }
      val rightTerms = for (m <- 0 until rank) yield {
        QuadraticTerm(-1, LinearTerm(0, new Map1(f(i, m, l), 1)), LinearTerm(0, new Map1(f(j, k, m), 1)))
      }
      ((i,j,k,l),Quadratic(LinearTerm[V](0, empty), leftTerms ++ rightTerms))
    }).toStream
  }

}