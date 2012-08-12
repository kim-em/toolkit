package net.tqft.toolkit.algebra

object IntegerPolynomialProgramming {

  // not exactly integer polynomial programming;
  // we try to find positive integer roots of the polynomials
  def solve[V](polynomials: Set[MultivariablePolynomial[Int, V]], variables: Set[V]): Iterable[Map[V, Int]] = {
    trait Solution {
      def remainingPolynomials: Set[MultivariablePolynomial[Int, V]]
      def values: Map[V, Int] 
    }
    
    trait Solver {
      def solutions: Iterable[Solution]
      def consider(p: MultivariablePolynomial[Int, V]): Solver = ???
    }
    
    case object EmptySolution extends Solution {
      override def remainingPolynomials = polynomials
      override def values = Map.empty      
    }
    
    case object EmptySolver extends Solver {
      override def solutions = Iterable(EmptySolution)
    }
    
    polynomials.foldLeft[Solver](EmptySolver)(_.consider(_)).solutions.map(_.values)
  }
  
}