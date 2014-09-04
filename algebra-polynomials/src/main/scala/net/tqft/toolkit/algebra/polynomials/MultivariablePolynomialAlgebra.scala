package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

trait MultivariablePolynomialAlgebra[A, V] extends Ring[MultivariablePolynomial[A, V]] with MultivariablePolynomialAlgebraOverRig[A, V] {
  implicit override def ring: Ring[A]

  override protected val implementation = new Ring.RingMap[Map[V, Int], A] {
    override def keys = implicitly[AdditiveGroup[Map[V, Int]]]
    override def coefficients = implicitly[Module[A, A]]
    override def multiplicativeCoefficients = implicitly[Ring[A]]
  }
  override def negate(p: MultivariablePolynomial[A, V]) = implementation.negate(p.coefficients)
}

object MultivariablePolynomialAlgebra {
  implicit def over[A: Ring, V: Ordering]: MultivariablePolynomialAlgebra[A, V] = new MultivariablePolynomialAlgebra[A, V] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
    override val variableOrdering = implicitly[Ordering[V]]
    override val ring = implicitly[Ring[A]]
  }
}

