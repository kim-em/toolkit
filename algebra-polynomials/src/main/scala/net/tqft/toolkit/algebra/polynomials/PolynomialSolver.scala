package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.Finite

trait PolynomialSolver[A] { solver =>
	def roots(p: Polynomial[A]): Map[A, Int]
	
	implicit class PolynomialRoots(p: Polynomial[A]) {
	  def roots = solver.roots(p)
	}
}

case class FiniteFieldPolynomialSolver[A:Finite]() extends PolynomialSolver[A] {
  override def roots(p: Polynomial[A]) = {
    ???
  }
}