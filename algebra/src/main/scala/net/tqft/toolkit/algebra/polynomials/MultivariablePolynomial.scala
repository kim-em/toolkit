package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.EuclideanRing
import net.tqft.toolkit.algebra.modules.LinearCombo

trait MultivariablePolynomial[A, V] extends LinearCombo[A, Map[V, Int]] {
  def totalDegree = {
    import net.tqft.toolkit.arithmetic.MinMax._
    terms.map(_._1.values.sum).maxOption
  }
  def termsOfDegree(k: Int) = terms.filter(_._1.values.sum == k)
  def constantTerm(implicit ring: Ring[A]) = terms.find(_._1.isEmpty).map(_._2).getOrElse(ring.zero)

  def nonZero = terms.nonEmpty
  lazy val variables = terms.flatMap(_._1.keySet).toSet

  def divideByCoefficientGCD(implicit euclideanDomain: EuclideanRing[A], ordering: Ordering[V]) = {
    val gcd = euclideanDomain.gcd(terms.map(_._2): _*)
    MultivariablePolynomial(terms.map(p => p.copy(_2 = euclideanDomain.quotient(p._2, gcd))): _*)
  }

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
    other match {
      case other: MultivariablePolynomial[_, _] => hashCode == other.hashCode && terms == other.terms
      case _ => false
    }
  }
  override lazy val hashCode: Int = terms.hashCode + 13

}

object MultivariablePolynomial {
  def apply[A: Ring, V: Ordering](terms: (Map[V, Int], A)*) = MultivariablePolynomialAlgebras.over.wrap(terms.toList)
  def unapplySeq[A, V](p: MultivariablePolynomial[A, V]) = Some(p.terms)
}
