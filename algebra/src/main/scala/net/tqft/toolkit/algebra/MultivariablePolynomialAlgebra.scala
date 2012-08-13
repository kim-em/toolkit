package net.tqft.toolkit.algebra

trait MultivariablePolynomialAlgebra[A, V] extends FreeModuleOnMonoid[A, Map[V, Int], MultivariablePolynomial[A, V]] with AssociativeAlgebra[A, MultivariablePolynomial[A, V]] {
  override def wrap(terms: List[(Map[V, Int], A)]): MultivariablePolynomial[A, V] = MultivariablePolynomialImpl(terms)
  private case class MultivariablePolynomialImpl(terms: List[(Map[V, Int], A)]) extends MultivariablePolynomial[A, V]

  override object monoid extends CommutativeMonoid[Map[V, Int]] {
    override val zero = Map.empty[V, Int]
    override def add(x: Map[V, Int], y: Map[V, Int]): Map[V, Int] = {
      (for (k <- x.keySet ++ y.keySet) yield {
        k -> Gadgets.Integers.add(x.getOrElse(k, 0), y.getOrElse(k, 0))
      }).toMap
    }
  }
  
  def substitute(values: Map[V, MultivariablePolynomial[A, V]])(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    add(for((m, a) <- p.terms) yield {
      val (toReplace, toKeep) = m.keySet.partition(v => values.contains(v))
      val newFactors = for(v <- toReplace.toSeq) yield power(values(v), m(v))
      val oldFactor = monomial(toKeep.map(v => v -> m(v)).toMap, a)
      multiply(oldFactor, newFactors:_*)
    })
  }
}

object MultivariablePolynomialAlgebras {
  def over[A: Ring, V]: MultivariablePolynomialAlgebra[A, V] = new MultivariablePolynomialAlgebra[A, V] {
    override val ring = implicitly[Ring[A]]
  }
}
