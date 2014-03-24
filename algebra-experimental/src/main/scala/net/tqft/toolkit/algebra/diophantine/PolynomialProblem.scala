//package net.tqft.toolkit.algebra.diophantine
//
//import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
//import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebra
//import net.tqft.toolkit.algebra.Rig
//
//case class PolynomialProblem[A: Rig, V: Ordering](substitutions: Map[V, MultivariablePolynomial[A, V]], equations: Seq[MultivariablePolynomial[A, V]]) {
//
//  private def polynomialAlgebra = MultivariablePolynomialAlgebra.overRig[A, V]
//  private type P = MultivariablePolynomial[A, V]
//
//  def addSubstitutions(newSubstitutions: Map[V, P]): PolynomialProblem[A, V] = {
//    require(newSubstitutions.keySet.intersect(substitutions.keySet).isEmpty)
//    def substitute(p: P) = polynomialAlgebra.substitute(newSubstitutions)(p)
//    PolynomialProblem(substitutions.mapValues(substitute) ++ newSubstitutions, equations.map(substitute))
//  }
//  def addEquations(newEquations: Seq[P], processor: P =>? Map[V, P] = Map.empty): PolynomialProblem[A, V] = {
//    newEquations.foldLeft(this)((pp, p) => pp.addEquation(p, processor))
//  }
//  private def addEquation(newEquation: P, processor: P =>? Map[V, P]): PolynomialProblem[A, V] = {
//    val e = polynomialAlgebra.substitute(substitutions)(newEquation)
//    processor.lift(e) match {
//      case None => {
//        copy(equations = equations :+ e)
//      }
//      case Some(newSubstitutions: Map[V, P]) => {
//        val (keep, reprocess) = equations.partition(f => f.variables.intersect(newSubstitutions.keySet).isEmpty)
//        PolynomialProblem(substitutions, keep).addSubstitutions(newSubstitutions).addEquations(reprocess, processor)
//      }
//    }
//  }
//}
//
//case class ChainingAlgorithm[A: Rig, V](locals: Seq[MultivariablePolynomial[A, V] => Option[Map[V, MultivariablePolynomial[A, V]]]]) extends PolynomialSolver[A] {
//  override def solve[V: Ordering](problem: PolynomialProblem[A, V]) = {
//    ???
//  }
//}
//
//trait PolynomialSolver[A] { solver =>
//  def solve[V: Ordering](problem: PolynomialProblem[A, V]): Iterable[PolynomialProblem[A, V]]
//}
//
//case class ConstantSolver[A]() extends PolynomialSolver[A] {
//  override def solve[V: Ordering](problem: PolynomialProblem[A, V]) = {
//    val (constants, others) = problem.equations.partition(???)
//    ???
//  }
//}
//
//// TODO A = B Solver
//
//// TODO the existing diophantine solver
//
//case class CaseBashSolver(core: PolynomialSolver[Int]) extends PolynomialSolver[Int] {
//  override def solve[V: Ordering](problem: PolynomialProblem[Int, V]) = ???
//} 