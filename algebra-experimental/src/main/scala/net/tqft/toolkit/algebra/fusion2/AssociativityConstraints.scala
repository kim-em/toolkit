package net.tqft.toolkit.algebra.fusion2

object AssociativityConstraints {

  def apply[V](rank: Int, f: (Int, Int, Int) => V): Seq[Quadratic[V]] = {
	 for(i <- 0 until rank; j <- 0 until rank; k <- 0 until rank; l <- 0 until rank) yield {
	   val leftTerms = for(m <- 0 until rank) yield {
	     QuadraticTerm(1, LinearTerm(0, Map(f(i,j,m) -> 1)), LinearTerm(0, Map(f(m,k,l)->1)))
	   }
	   val rightTerms = for(m <- 0 until rank) yield {
	     QuadraticTerm(-1, LinearTerm(0, Map(f(i,m,l) -> 1)), LinearTerm(0, Map(f(j,k,m)->1)))
	   }
	   Quadratic(LinearTerm[V](0, Map.empty), leftTerms ++ rightTerms)
	 }
  }
  
  def apply(numberOfSelfDualObjects: Int, numberOfDualPairs: Int): Seq[Quadratic[(Int, Int, Int)]] = {
    val rank = numberOfSelfDualObjects + 2 * numberOfDualPairs
    def dual(i: Int) = {
      if(i <= numberOfSelfDualObjects) {
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
    val equations = apply[(Int, Int, Int)](rank, f _)
    val evaluatedEquations = (for(i <- 0 until rank; j <- 0 until rank; k = if(i == dual(j)) 1 else  0) yield (i,j,k)).foldLeft(equations)({ case (equations, (i, j, k)) => equations.map(e => e.substitute(f(i,j,0), k))})
    evaluatedEquations.filter(e => !e.zero_?)
  }
  
}