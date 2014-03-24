package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.EuclideanRing
import net.tqft.toolkit.algebra.modules.MapLinearCombo
import net.tqft.toolkit.algebra.modules.LinearCombo
import net.tqft.toolkit.algebra.Rig
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.OrderedField
import net.tqft.toolkit.algebra.OrderedEuclideanRing

import scala.language.implicitConversions

trait MultivariablePolynomial[A, V] extends MapLinearCombo[A, Map[V, Int]] {
  def totalDegree = {
    import net.tqft.toolkit.arithmetic.MinMax._
    toMap.keys.map(_.values.sum).maxOption
  }
  def termsOfDegree(k: Int) = toMap.filterKeys(_.values.sum == k)
  def constantTerm(implicit ring: Rig[A]) = toMap.getOrElse(Map.empty, ring.zero)

  def nonZero = toMap.nonEmpty
  lazy val variables = toMap.keySet.flatMap(_.keySet)

  def divideByCoefficientGCD(implicit euclideanDomain: EuclideanRing[A], ordering: Ordering[V]) = {
    val gcd = euclideanDomain.gcd(toMap.values.toSeq: _*)
    if (gcd == euclideanDomain.one) {
      this
    } else {
      MultivariablePolynomial(toMap.mapValues(v => euclideanDomain.quotient(v, gcd)))
    }
  }

  override lazy val toString = {
    if (toMap.isEmpty) {
      "0"
    } else {
      toSeq.map({
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
  implicit def apply[A: Ring, V: Ordering](m: Map[Map[V, Int], A]) = MultivariablePolynomialAlgebra.over[A, V].wrap(m)

  implicit def multivariablePolynomialAlgebraAsRig[A: Rig, V: Ordering]: Rig[MultivariablePolynomial[A, V]] = implicitly[MultivariablePolynomialAlgebraOverRig[A, V]]
  implicit def multivariablePolynomialAlgebraAsRing[A: Ring, V: Ordering]: Ring[MultivariablePolynomial[A, V]] = implicitly[MultivariablePolynomialAlgebra[A, V]]
  implicit def multivariablePolynomialAlgebraAsEuclideanRing[A: Field, V: Ordering]: EuclideanRing[MultivariablePolynomial[A, V]] = implicitly[MultivariablePolynomialAlgebraOverField[A, V]]
}
