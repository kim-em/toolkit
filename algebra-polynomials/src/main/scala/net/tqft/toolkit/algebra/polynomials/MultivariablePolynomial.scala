package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import scala.language.implicitConversions
import scala.collection.SortedMap

case class MultivariablePolynomial[A, V](coefficients: Map[Map[V, Int], A]) {
  //  require(coefficients.valuesIterator.forall(_ != implicitly[Rig[A]].zero))
  //  require(coefficients.valuesIterator.forall(_.toString != "0"))
  //  require(coefficients.valuesIterator.forall(_.toString != "Fraction(0, 1)"))
  //  require(coefficients.keysIterator.forall(_.valuesIterator.forall(_ >= 0)))
  def mapValues[B](f: A => B) = MultivariablePolynomial(coefficients.mapValues(f))
}

object MultivariablePolynomial {
  implicit def lift[A, V](coefficients: Map[Map[V, Int], A]) = MultivariablePolynomial[A, V](coefficients)

  implicit def liftCoefficientsToFractions[A: Rig, V](coefficients: Map[Map[V, Int], A]): MultivariablePolynomial[Fraction[A], V] = MultivariablePolynomial(coefficients.mapValues(a => a))
  implicit def liftCoefficientsToBigInts[V](coefficients: Map[Map[V, Int], Int]): MultivariablePolynomial[BigInt, V] = MultivariablePolynomial(coefficients.mapValues(a => a))
  implicit def liftCoefficientsToPolynomials[A: Rig, V](coefficients: Map[Map[V, Int], A]): MultivariablePolynomial[Polynomial[A], V] = lift(coefficients.mapValues(a => a))
  
  implicit def liftVariable[A: Rig, V: Ordering](v: V): MultivariablePolynomial[A, V] = implicitly[MultivariablePolynomialAlgebraOverRig[A, V]].monomial(v)
  
  implicit def constant[A: Rig, V](a: A): MultivariablePolynomial[A, V] = {
    if (implicitly[Rig[A]].zero_?(a)) {
      MultivariablePolynomial(Map.empty)
    } else {
      MultivariablePolynomial(new scala.collection.immutable.Map.Map1(Map.empty, a))
    }
  }
  implicit def bigIntConstant[V](a: Int): MultivariablePolynomial[BigInt, V] = {
    if (a == 0) {
      MultivariablePolynomial(Map.empty)
    } else {
      MultivariablePolynomial(new scala.collection.immutable.Map.Map1(Map.empty, a))
    }
  }
  implicit def bigIntConstantFraction[V](a: Int): MultivariablePolynomial[Fraction[BigInt], V] = {
    if (a == 0) {
      MultivariablePolynomial(Map.empty)
    } else {
      MultivariablePolynomial(new scala.collection.immutable.Map.Map1(Map.empty, Fraction.whole(a)))
    }
  }

  implicit def constantFraction[A: GCDRing, V](a: A): MultivariablePolynomial[Fraction[A], V] = MultivariablePolynomial(Map(Map.empty -> (a: Fraction[A])))

  implicit def constantRationalFunction[A: GCDRing, V: Ordering](a: A): MultivariableRationalFunction[A, V] = MultivariablePolynomial[A, V](Map(Map.empty -> a))
  implicit def constantToFractionRationalFuncation[A: GCDRing, V: Ordering](a: A): MultivariableRationalFunction[Fraction[A], V] = constantRationalFunction(a)
  implicit def liftToRationalFunction[A: GCDRing, V: Ordering](coefficients: Map[Map[V, Int], A]): MultivariableRationalFunction[A, V] = lift(coefficients)
  implicit def liftCoefficientsToFractions[A: GCDRing, V: Ordering](coefficients: Map[Map[V, Int], A]): MultivariableRationalFunction[Fraction[A], V] = liftToRationalFunction(coefficients.mapValues(a => (a: Fraction[A])))
  implicit def bigIntConstantRationalFunction[V: Ordering](a: Int): MultivariableRationalFunction[BigInt, V] = Fraction.whole(bigIntConstant[V](a))
  implicit def bigIntConstantFractionRationalFunction[V: Ordering](a: Int): MultivariableRationalFunction[Fraction[BigInt], V] = Fraction.whole(bigIntConstantFraction[V](a))
  implicit def bigIntRationalFunction[V: Ordering](coefficients: Map[Map[V, Int], Int]): MultivariableRationalFunction[BigInt, V] = coefficients.mapValues(a => a: BigInt)
  implicit def bigIntFractionRationalFunction[V: Ordering](coefficients: Map[Map[V, Int], Int]): MultivariableRationalFunction[Fraction[BigInt], V] = coefficients.mapValues(a => a: BigInt)

  implicit def degreeLexicographicOrdering[A: Ordering: Zero, V: Ordering]: Ordering[MultivariablePolynomial[A, V]] = {
    val zero = implicitly[Zero[A]].zero
    val aOrdering = implicitly[Ordering[A]]
    new Ordering[MultivariablePolynomial[A, V]] {
      override def compare(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]): Int = {
        val t = x.coefficients.keys.map(_.values.sum).max - y.coefficients.keys.map(_.values.sum).max
        if (t != 0) {
          t
        } else {
          import net.tqft.toolkit.orderings.LexicographicOrdering.degreeLexicographic
          val keys = (x.coefficients.keySet ++ y.coefficients.keySet).toSeq.sorted
          for (key <- keys) {
            val c = aOrdering.compare(x.coefficients.get(key).getOrElse(zero), y.coefficients.get(key).getOrElse(zero))
            if (c != 0) return c
          }
          0
        }
      }
    }
  }

  implicit class RichMultivariablePolynomial[A, V](m: MultivariablePolynomial[A, V]) {
    def variables = m.coefficients.keySet.flatMap(_.keySet)
    def constantTerm(implicit ring: Ring[A]): A = m.coefficients.get(Map.empty).getOrElse(implicitly[Ring[A]].zero)
    def totalDegree = {
      import net.tqft.toolkit.arithmetic.MinMax._
      m.coefficients.keys.map(_.values.sum).maxOption
    }
    def termsOfDegree(k: Int) = m.coefficients.filterKeys(_.values.sum == k)
  }

  implicit def multivariablePolynomialAlgebraAsRig[A: Rig, V: Ordering]: Rig[MultivariablePolynomial[A, V]] = implicitly[MultivariablePolynomialAlgebraOverRig[A, V]]
  implicit def multivariablePolynomialAlgebraAsRing[A: Ring, V: Ordering]: Ring[MultivariablePolynomial[A, V]] = implicitly[MultivariablePolynomialAlgebra[A, V]]
  implicit def multivariablePolynomialAlgebraAsGCDRing[A: GCDRing, V: Ordering]: GCDRing[MultivariablePolynomial[A, V]] = implicitly[MultivariablePolynomialAlgebraOverGCDRing[A, V]]
}
