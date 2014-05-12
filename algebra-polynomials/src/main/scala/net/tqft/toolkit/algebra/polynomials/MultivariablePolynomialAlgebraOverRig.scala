package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

trait MultivariablePolynomialAlgebraOverRig[A, V] extends Rig[MultivariablePolynomial[A, V]] with ModuleOverRig[A, MultivariablePolynomial[A, V]] {
  implicit def ring: Rig[A]

  implicit def variableOrdering: Ordering[V]

  protected val implementation = new Rig.RigMap[Map[V, Int], A] {
    override def keys = implicitly[AdditiveGroup[Map[V, Int]]]
    override def coefficients = implicitly[ModuleOverRig[A, A]]
    override def multiplicativeCoefficients = implicitly[Rig[A]]
  }

  override def one = implementation.one
  override def zero = implementation.zero
  override def add(p1: MultivariablePolynomial[A, V], p2: MultivariablePolynomial[A, V]) = implementation.add(p1.coefficients, p2.coefficients)
  override def multiply(p1: MultivariablePolynomial[A, V], p2: MultivariablePolynomial[A, V]) = implementation.multiply(p1.coefficients, p2.coefficients)
  override def scalarMultiply(a: A, p: MultivariablePolynomial[A, V]) = implementation.scalarMultiply(a, p.coefficients)

  override def fromInt(k: Int) = constant(ring.fromInt(k))

  implicit val monomialOrdering: Ordering[Map[V, Int]]

  def constant(a: A) = monomial(Map.empty, a)
  def monomial(v: V): MultivariablePolynomial[A, V] = Map(Map(v -> 1) -> ring.one)
  def monomial(m: Map[V, Int]): MultivariablePolynomial[A, V] = Map(m -> ring.one)
  def monomial(m: Map[V, Int], a: A): MultivariablePolynomial[A, V] = {
    if (a == ring.zero) {
      Map.empty[Map[V, Int], A]
    } else {
      Map(m -> a)
    }
  }

  def constantTerm(p: MultivariablePolynomial[A, V]): A = {
    p.coefficients.get(Map()).getOrElse(ring.zero)
  }
  def leadingMonomial(p: MultivariablePolynomial[A, V]): Option[Map[V, Int]] = {
    import net.tqft.toolkit.arithmetic.MinMax._
//    p.coefficients.keySet.maxOption(monomialOrdering)
    p.coefficients.map(_._1).toSeq.sorted(monomialOrdering).lastOption
  }
  def coefficientOfOneVariable(v: V, i: Int)(x: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    x.coefficients.collect({
      case (m, a) if m.getOrElse(v, 0) == i => (m - v, a)
    })
  }
  def maximumDegreeOfVariable(v: V)(x: MultivariablePolynomial[A, V]): Option[Int] = {
    import net.tqft.toolkit.arithmetic.MinMax._
    x.coefficients.keys.flatMap(_.get(v)).maxOption match {
      case Some(d) => Some(d)
      case None => {
        x.coefficients.find(p => p._2 != ring.zero).map(_ => 0)
      }
    }
  }
  def asUnivariatePolynomialInVariable(v: V)(x: MultivariablePolynomial[A, V]): Polynomial[MultivariablePolynomial[A, V]] = {
    val exponents = 0 +: x.coefficients.keys.flatMap(_.get(v)).toSeq
    val result = (for (
      k <- 0 +: x.coefficients.keys.flatMap(_.get(v)).toSeq;
      c = coefficientOfOneVariable(v, k)(x);
      if c != zero
    ) yield {
      k -> c
    }).toMap[Int, MultivariablePolynomial[A, V]]

    implicit def zero_ = this

    result
  }
  def fromUnivariatePolynomialInVariable(v: V)(p: Polynomial[MultivariablePolynomial[A, V]]): MultivariablePolynomial[A, V] = {
    sum(p.toMap.map({
      case (k, q) => multiply(monomial(Map(v -> k)), q)
    }))
  }
  def variables(x: MultivariablePolynomial[A, V]): Set[V] = {
    x.coefficients.keySet.flatMap(_.keySet)
  }
  def variablesByMaximumDegree(x: MultivariablePolynomial[A, V]): SortedMap[Int, Set[V]] = {
    // TODO This could be made more efficient
    TreeMap[Int, Set[V]]() ++ variables(x).groupBy(v => maximumDegreeOfVariable(v)(x).get)
  }

  def substitute(values: Map[V, MultivariablePolynomial[A, V]])(p: MultivariablePolynomial[A, V]) = substitute_(values, p)

  private def substitute_(values: Map[V, MultivariablePolynomial[A, V]], p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    import MultivariablePolynomial._

    val relevantValues = values.filterKeys(p.variables.contains)
    if (relevantValues.isEmpty) {
      p
    } else {
      sum(for ((m, a) <- p.coefficients.iterator) yield {
        val (toReplace, toKeep) = m.keySet.partition(v => relevantValues.contains(v))
        if (toReplace.isEmpty) {
          Map(m -> a)
        } else {
          val newFactors = for (v <- toReplace.toSeq) yield power(relevantValues(v), m(v))
          if (toKeep.isEmpty) {
            scalarMultiply(a, product(newFactors))
          } else {
            val oldFactor = monomial(toKeep.map(v => v -> m(v)).toMap, a)
            multiply(oldFactor, product(newFactors))
          }
        }
      })
    }
  }
  def substituteConstants(map: Map[V, A])(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    substitute(map.mapValues(a => Map(Map.empty[V, Int] -> a)))(p)
  }
  def completelySubstituteConstants(map: V =>? A)(p: MultivariablePolynomial[A, V]): A = {
    val mapWithZero = map.lift.andThen(_.getOrElse(ring.zero))

    ring.sum(for ((m, a) <- p.coefficients.iterator) yield {
      ring.multiply(a, ring.product(for ((v, k) <- m) yield {
        ring.power(mapWithZero(v), k)
      }))
    })
  }

  trait Ideal {
    def generators: Seq[MultivariablePolynomial[A, V]]
  }

}

object MultivariablePolynomialAlgebraOverRig {
  trait LexicographicOrdering[A, V] { self: MultivariablePolynomialAlgebraOverRig[A, V] =>
    override lazy val monomialOrdering = {
      import net.tqft.toolkit.orderings.LexicographicOrdering._
      implicitly[Ordering[Map[V, Int]]]
    }
  }

  trait DegreeLexicographicOrdering[A, V] { self: MultivariablePolynomialAlgebraOverRig[A, V] =>
    override lazy val monomialOrdering = {
      import net.tqft.toolkit.orderings.LexicographicOrdering._
      import net.tqft.toolkit.orderings.Orderings._
      Ordering
        .by({ m: Map[V, Int] => m.values.sum })
        .refineAlong(implicitly[Ordering[Map[V, Int]]])
    }
  }

  implicit def overRig[A: Rig, V: Ordering]: MultivariablePolynomialAlgebraOverRig[A, V] = new MultivariablePolynomialAlgebraOverRig[A, V] with LexicographicOrdering[A, V] {
    override val variableOrdering = implicitly[Ordering[V]]
    override val ring = implicitly[Rig[A]]
  }
}
