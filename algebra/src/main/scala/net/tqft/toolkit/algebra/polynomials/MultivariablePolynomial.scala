package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.EuclideanRing
import net.tqft.toolkit.algebra.modules.LinearCombo
import net.tqft.toolkit.algebra.Rig

trait MultivariablePolynomial[A, V] extends Map[Map[V, Int], A] {
  def totalDegree = {
    import net.tqft.toolkit.arithmetic.MinMax._
    keys.map(_.values.sum).maxOption
  }
  def termsOfDegree(k: Int) = filterKeys(_.values.sum == k)
  def constantTerm(implicit ring: Rig[A]) = getOrElse(Map.empty, ring.zero)

  def nonZero = nonEmpty
  lazy val variables = keySet.flatMap(_.keySet)

  def divideByCoefficientGCD(implicit euclideanDomain: EuclideanRing[A], ordering: Ordering[V]) = {
    val gcd = euclideanDomain.gcd(values.toSeq:_*)
    if (gcd == euclideanDomain.one) {
      this
    } else {
      MultivariablePolynomial(mapValues(v => euclideanDomain.quotient(v, gcd)))
    }
  }

  override lazy val toString = {
    if(isEmpty) {
      "0"
    } else {
      toList.map({
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
      case other: MultivariablePolynomial[_, _] => hashCode == other.hashCode && super.equals(other)
      case _ => false
    }
  }
  override lazy val hashCode: Int = super.hashCode

}

object MultivariablePolynomial {
//  def apply[A: Ring, V: Ordering](terms: (Map[V, Int], A)*) = MultivariablePolynomialAlgebra.over.wrap(terms.toList)
  def apply[A: Ring, V: Ordering](m: Map[Map[V, Int], A]) = MultivariablePolynomialAlgebra.over.wrap(m)
//  def unapplySeq[A, V](p: MultivariablePolynomial[A, V]) = Some(p.terms)
}
