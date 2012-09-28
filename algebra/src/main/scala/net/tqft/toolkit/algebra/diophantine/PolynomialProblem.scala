package net.tqft.toolkit.algebra.diophantine

import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial

case class PolynomialProblem[A, V](solutions: Map[V, A], substitutions: Map[V, MultivariablePolynomial[A, V]], equations: Seq[MultivariablePolynomial[A, V]])

trait PolynomialSolver[A] { solver =>
  def solve[V](problem: PolynomialProblem[A, V]): Iterable[PolynomialProblem[A, V]]
  
  def andThen(next: PolynomialSolver[A]): PolynomialSolver[A] = new PolynomialSolver[A] {
    override def solve[V](problem: PolynomialProblem[A, V]) = solver.solve(problem).flatMap(p => next.solve(p))
  }
}

case class ConstantSolver[A]() extends PolynomialSolver[A] {
  override def solve[V](problem: PolynomialProblem[A, V]) = {
    val (constants, others) = problem.equations.partition(???)
    ???
  }
}

// TODO A = B Solver

// TODO the existing diophantine solver

case class CaseBashSolver(core: PolynomialSolver[Int]) extends PolynomialSolver[Int] {
  override def solve[V](problem: PolynomialProblem[Int, V]) = ???
} 