package net.tqft.toolkit.algebra

object IntegerPolynomialProgramming {

  // not exactly integer polynomial programming;
  // we try to find positive integer roots of the polynomials
  def solve[V](polynomials: Set[MultivariablePolynomial[Int, V]], variables: Set[V]): Iterable[Map[V, Int]] = {
    
    val polynomialAlgebra = MultivariablePolynomialAlgebras.over[Int, V](Gadgets.Integers)
    
    trait SolveStrategy {
      def solutions: Map[V, Int]
      def substitutions: Map[V, MultivariablePolynomial[Int, V]]
      def polynomials: Set[MultivariablePolynomial[Int, V]]
      def consider(p: MultivariablePolynomial[Int, V]): Iterable[SolveStrategy] 
      
      protected def simplify(p: MultivariablePolynomial[Int, V]): MultivariablePolynomial[Int, V] = {
        polynomialAlgebra.substitute(solutions.mapValues(polynomialAlgebra.constant(_)) ++ substitutions)(p)
      }
    }
    
    case class TrivialSolver(solutions: Map[V, Int], substitutions: Map[V, MultivariablePolynomial[Int, V]], polynomials: Set[MultivariablePolynomial[Int, V]]) extends SolveStrategy {
      override def consider(p: MultivariablePolynomial[Int, V]): Iterable[SolveStrategy] = Iterable(copy(polynomials = polynomials + p))
    }
    
    case class EasySolver(solutions: Map[V, Int], substitutions: Map[V, MultivariablePolynomial[Int, V]], polynomials: Set[MultivariablePolynomial[Int, V]]) extends SolveStrategy {
      override def consider(p: MultivariablePolynomial[Int, V]): Iterable[EasySolver] = {
        ???
      }
    }
    
    val strategies: List[(Map[V,Int], Map[V, MultivariablePolynomial[Int, V]], Set[MultivariablePolynomial[Int, V]]) => SolveStrategy] = List(TrivialSolver, EasySolver)
    
    val empty: SolveStrategy = strategies.head(Map.empty, Map.empty, Set.empty)
    
    val finished = polynomials.foldLeft(Iterable(empty))((i, p) => i.flatMap(_.consider(p)))
    for(s <- finished; if s.polynomials.isEmpty) yield s.solutions
      
  }

}