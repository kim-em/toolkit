package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.modules._

trait MultivariablePolynomialAlgebraOverRig[A, V]
  extends FreeModuleOnMonoidOverRig[A, Map[V, Int], MultivariablePolynomial[A, V]] {

  val monomialOrdering: Ordering[Map[V, Int]]

  override def wrap(terms: Seq[(Map[V, Int], A)]): MultivariablePolynomial[A, V] = {
    MultivariablePolynomialImpl(terms.sortBy(_._1)(monomialOrdering))
  }
  private case class MultivariablePolynomialImpl(terms: Seq[(Map[V, Int], A)]) extends MultivariablePolynomial[A, V]

  def monomial(v: V): MultivariablePolynomial[A, V] = MultivariablePolynomialImpl(Seq((Map(v -> 1), ring.one)))
  override def monomial(m: Map[V, Int]): MultivariablePolynomial[A, V] = MultivariablePolynomialImpl(Seq((m, ring.one)))
  override def monomial(m: Map[V, Int], a: A): MultivariablePolynomial[A, V] = {
    if (a == ring.zero) {
      MultivariablePolynomialImpl(Seq())
    } else {
      MultivariablePolynomialImpl(Seq((m, a)))
    }
  }

  override object monoid extends AdditiveMonoid[Map[V, Int]] {
    override val zero = Map.empty[V, Int]
    override def add(x: Map[V, Int], y: Map[V, Int]): Map[V, Int] = {
      (for (k <- x.keySet ++ y.keySet) yield {
        k -> Integers.add(x.getOrElse(k, 0), y.getOrElse(k, 0))
      }).toMap
    }
  }

  def substitute(values: Map[V, MultivariablePolynomial[A, V]])(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    val relevantValues = values.filterKeys(p.variables.contains)
    if (relevantValues.isEmpty) {
      p
    } else {
      sum(for ((m, a) <- p.terms) yield {
        val (toReplace, toKeep) = m.keySet.partition(v => relevantValues.contains(v))
        val newFactors = for (v <- toReplace.toSeq) yield power(relevantValues(v), m(v))
        val oldFactor = monomial(toKeep.map(v => v -> m(v)).toMap, a)
        multiply(oldFactor, newFactors: _*)
      })
    }
  }
  def substituteConstants(values: Map[V, A])(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    substitute(values.mapValues(constant))(p)
  }
  def completelySubstituteConstants(values: V =>? A)(p: MultivariablePolynomial[A, V]): A = {
    val valuesWithZero = values.lift.andThen(_.getOrElse(ring.zero))
    
    ring.sum(for((m, a) <- p.terms) yield {
      ring.multiply(a, ring.product(for((v, k) <- m) yield {
        ring.power(valuesWithZero(v), k)
      }))
    })    
  }

}

trait MultivariablePolynomialAlgebra[A, V]
  extends MultivariablePolynomialAlgebraOverRig[A, V]
  with FreeModuleOnMonoid[A, Map[V, Int], MultivariablePolynomial[A, V]]
  with AssociativeAlgebra[A, MultivariablePolynomial[A, V]]

object MultivariablePolynomialAlgebra {
  private abstract class WithLexicographicOrdering[A: Rig, V: Ordering] extends MultivariablePolynomialAlgebraOverRig[A, V] {
    override val monomialOrdering = {
      import net.tqft.toolkit.collections.LexicographicOrdering._
      implicitly[Ordering[Map[V, Int]]]
    }
  }

  def overRig[A: Rig, V: Ordering]: MultivariablePolynomialAlgebraOverRig[A, V] = new WithLexicographicOrdering[A, V] {
    override val ring = implicitly[Rig[A]]
  }
  implicit def over[A: Ring, V: Ordering]: MultivariablePolynomialAlgebra[A, V] = new WithLexicographicOrdering[A, V] with MultivariablePolynomialAlgebra[A, V] {
    override val ring = implicitly[Ring[A]]
  }
}
