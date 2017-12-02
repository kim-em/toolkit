package net.tqft.toolkit.algebra.polynomials

trait GroebnerBasis[A, V] {
	def apply(polynomials: Seq[MultivariablePolynomial[A, V]])(implicit ordering: Ordering[V]): Seq[MultivariablePolynomial[A, V]]
}

trait GroebnerBasisOperations {
  implicit class operation[A, V](polynomials: Seq[MultivariablePolynomial[A, V]])(implicit algorithm: GroebnerBasis[A, V]) {
    def computeGroebnerBasis(implicit ordering: Ordering[V]) = algorithm(polynomials)
  }  
}