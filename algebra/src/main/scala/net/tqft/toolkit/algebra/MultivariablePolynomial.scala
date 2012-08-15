package net.tqft.toolkit.algebra

trait MultivariablePolynomial[A, V] extends LinearCombo[A, Map[V, Int]] {
  def totalDegree = {
    import net.tqft.toolkit.arithmetic.MinMax._
    terms.map(_._1.values.sum).maxOption
  }
  def termsOfDegree(k: Int) = terms.filter(_._1.values.sum == k)
  def constantTerm(implicit ring: Ring[A]) = terms.find(_._1.isEmpty).map(_._2).getOrElse(ring.zero)

  def nonZero = terms.nonEmpty
  def variables = terms.flatMap(_._1.keySet).toSet

  override def toString = {
    terms match {
      case Nil => "0"
      case _ => terms.map({
        case (m, a) => {
          val showCoefficient = m.isEmpty || a.toString != "1"
          val showMonomial = m.nonEmpty
          (if (showCoefficient) a.toString else "") + (if (showCoefficient && showMonomial) " * " else "") + (if (showMonomial) { m.map({ case (v, k) => v + (if (k > 1) "^" + k else "") }).mkString(" * ") } else "")
        }
      }).mkString(" + ")
    }
  }
  override def equals(other: Any) = {
    val result = other match {
      case other: MultivariablePolynomial[_, _] => terms == other.terms
      case _ => false
    }
    if(result == false) require(toString != other.toString)
    result
  }
  override lazy val hashCode: Int = terms.hashCode

}

object MultivariablePolynomial {
  def apply[A: Ring, V: Ordering](terms: (Map[V, Int], A)*) = MultivariablePolynomialAlgebras.over.wrap(terms.toList)
  def unapplySeq[A, V](p: MultivariablePolynomial[A, V]) = Some(p.terms)
}
