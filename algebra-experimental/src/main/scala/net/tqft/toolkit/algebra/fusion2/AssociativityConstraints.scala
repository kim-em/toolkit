package net.tqft.toolkit.algebra.fusion2

object AssociativityConstraints {

  private def quadratics[V](rank: Int, f: (Int, Int, Int) => V): Seq[Quadratic[V]] = {
	 (for(i <- (0 until rank).iterator; j <- (0 until rank).iterator; k <- (0 until rank).iterator; l <- (0 until rank).iterator) yield {
	   val leftTerms = for(m <- 0 until rank) yield {
	     QuadraticTerm(1, LinearTerm(0, Map(f(i,j,m) -> 1)), LinearTerm(0, Map(f(m,k,l)->1)))
	   }
	   val rightTerms = for(m <- 0 until rank) yield {
	     QuadraticTerm(-1, LinearTerm(0, Map(f(i,m,l) -> 1)), LinearTerm(0, Map(f(j,k,m)->1)))
	   }
	   Quadratic(LinearTerm[V](0, Map.empty), leftTerms ++ rightTerms)
	 }).toStream
  }
  
  
  
  def quadratics(numberOfSelfDualObjects: Int, numberOfDualPairs: Int): Seq[Quadratic[(Int, Int, Int)]] = {
    val rank = numberOfSelfDualObjects + 2 * numberOfDualPairs
    def dual(i: Int) = {
      if(i < numberOfSelfDualObjects) {
        i
      } else {
        if((i - numberOfSelfDualObjects) % 2 == 0) {
          i + 1
        } else {
          i - 1
        }
      }
    }
    def f(a: Int, b: Int, c: Int) = {
      Seq((c, dual(b), a), (dual(a), c, b), (a, b, c), (b, dual(c), dual(a)), (dual(c), a, dual(b)), (dual(b), dual(a), dual(c))).min
    }
    val dualitySubstitutions = (for(i <- 0 until rank; j <- 0 until rank; k = if(i == dual(j)) 1 else  0) yield (f(i,j,0),k))
    val equations = quadratics[(Int, Int, Int)](rank, f _)
    val evaluatedEquations = dualitySubstitutions.foldLeft(equations)({ case (equations, (p, k)) => equations.map(e => e.substitute(p, k))})
    evaluatedEquations.map(_.factor).filter(e => !e.zero_?)
  }
  
  def apply(numberOfSelfDualObjects: Int, numberOfDualPairs: Int) = SystemOfQuadratics(quadratics(numberOfSelfDualObjects, numberOfDualPairs).map(q => q: QuadraticHistory[(Int, Int, Int)]))
}