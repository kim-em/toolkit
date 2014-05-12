package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.Finite
import net.tqft.toolkit.algebra.Zero

trait PolynomialSolver[A] { solver =>
  def roots(p: Polynomial[A]): Map[A, Int]

  implicit class PolynomialRoots(p: Polynomial[A]) {
    def roots = solver.roots(p)
  }
}

case class FiniteFieldPolynomialSolver[A: Finite:  Polynomials]() extends PolynomialSolver[A] {
  private def polynomials = implicitly[Polynomials[A]]
  private def zero = polynomials.ring.zero
  
  // FIXME is this actually correct? p-th roots and all that
  override def roots(p: Polynomial[A]) = {
    val rootsWithoutMultiplicities = implicitly[Finite[A]].elements.filter(a => polynomials.evaluateAt(a)(p) == zero)
    rootsWithoutMultiplicities.map({ a =>
      a -> Iterator.iterate(p)(polynomials.formalDerivative).zipWithIndex.find(p => polynomials.evaluateAt(a)(p._1) != zero).get._2
    }).toMap
  }
}