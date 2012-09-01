package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.modules._

trait MultivariablePolynomialAlgebra[A, V] extends FreeModuleOnMonoid[A, Map[V, Int], MultivariablePolynomial[A, V]] with AssociativeAlgebra[A, MultivariablePolynomial[A, V]] {
  val monomialOrdering: Ordering[Map[V, Int]]

  override def wrap(terms: Seq[(Map[V, Int], A)]): MultivariablePolynomial[A, V] = {
    MultivariablePolynomialImpl(terms.sortBy(_._1)(monomialOrdering))
  }
  private case class MultivariablePolynomialImpl(terms: Seq[(Map[V, Int], A)]) extends MultivariablePolynomial[A, V]

  override object monoid extends CommutativeMonoid[Map[V, Int]] {
    override val zero = Map.empty[V, Int]
    override def add(x: Map[V, Int], y: Map[V, Int]): Map[V, Int] = {
      (for (k <- x.keySet ++ y.keySet) yield {
        k -> Gadgets.Integers.add(x.getOrElse(k, 0), y.getOrElse(k, 0))
      }).toMap
    }
  }

  def monomial(v: V): MultivariablePolynomial[A, V] = MultivariablePolynomialImpl(Seq((Map(v -> 1), ring.one)))
  override def monomial(m: Map[V, Int]): MultivariablePolynomial[A, V] = MultivariablePolynomialImpl(Seq((m, ring.one)))
  override def monomial(m: Map[V, Int], a: A): MultivariablePolynomial[A, V] = {
    if (a == ring.zero) {
      MultivariablePolynomialImpl(Seq())
    } else {
      MultivariablePolynomialImpl(Seq((m, a)))
    }
  }

  def substitute(values: Map[V, MultivariablePolynomial[A, V]])(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    add(for ((m, a) <- p.terms) yield {
      val (toReplace, toKeep) = m.keySet.partition(v => values.contains(v))
      val newFactors = for (v <- toReplace.toSeq) yield power(values(v), m(v))
      val oldFactor = monomial(toKeep.map(v => v -> m(v)).toMap, a)
      multiply(oldFactor, newFactors: _*)
    })
  }
  def substituteConstants(values: Map[V, A])(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = substitute(values.mapValues(constant))(p)

}

object MultivariablePolynomialAlgebras {
  def over[A: Ring, V: Ordering]: MultivariablePolynomialAlgebra[A, V] = new MultivariablePolynomialAlgebra[A, V] {
    override val ring = implicitly[Ring[A]]
    override val monomialOrdering = {
      import net.tqft.toolkit.collections.LexicographicOrdering._
      implicitly[Ordering[Map[V, Int]]]
    }
  }
}
