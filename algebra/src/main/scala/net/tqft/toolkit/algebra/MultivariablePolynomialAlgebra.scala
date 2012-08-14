package net.tqft.toolkit.algebra

trait MultivariablePolynomialAlgebra[A, V] extends FreeModuleOnMonoid[A, Map[V, Int], MultivariablePolynomial[A, V]] with AssociativeAlgebra[A, MultivariablePolynomial[A, V]] {
  val monomialOrdering: Ordering[Map[V, Int]]

  override def wrap(terms: List[(Map[V, Int], A)]): MultivariablePolynomial[A, V] = {
    val nonZeroTerms = for((m, a) <- terms; if a != ring.zero) yield {
      for(v <- m.keySet) require(m(v) > 0)
      (m, a)
    }
    MultivariablePolynomialImpl(nonZeroTerms.sortBy(_._1)(monomialOrdering))
  }
  private case class MultivariablePolynomialImpl(terms: List[(Map[V, Int], A)]) extends MultivariablePolynomial[A, V]

  override object monoid extends CommutativeMonoid[Map[V, Int]] {
    override val zero = Map.empty[V, Int]
    override def add(x: Map[V, Int], y: Map[V, Int]): Map[V, Int] = {
      (for (k <- x.keySet ++ y.keySet) yield {
        k -> Gadgets.Integers.add(x.getOrElse(k, 0), y.getOrElse(k, 0))
      }).toMap
    }
  }

  def monomial(v: V): MultivariablePolynomial[A, V] = monomial(Map(v -> 1))

  def substitute(values: Map[V, MultivariablePolynomial[A, V]])(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    add(for ((m, a) <- p.terms) yield {
      val (toReplace, toKeep) = m.keySet.partition(v => values.contains(v))
      val newFactors = for (v <- toReplace.toSeq) yield power(values(v), m(v))
      val oldFactor = monomial(toKeep.map(v => v -> m(v)).toMap, a)
      multiply(oldFactor, newFactors: _*)
    })
  }
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
