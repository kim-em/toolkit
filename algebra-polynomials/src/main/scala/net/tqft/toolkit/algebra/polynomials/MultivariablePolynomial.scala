package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

import scala.language.implicitConversions

case class MultivariablePolynomial[A, V](coefficients: Map[Map[V, Int], A]) {
  require(coefficients.valuesIterator.forall(_.toString != "0"))
  require(coefficients.valuesIterator.forall(_.toString != "Fraction(0, 1)"))
  require(coefficients.keysIterator.forall(_.valuesIterator.forall(_ >= 0)))
}

object MultivariablePolynomial {
  implicit def lift[A, V](coefficients: Map[Map[V, Int], A]) = MultivariablePolynomial[A, V](coefficients)

  implicit def liftCoefficientsToFractions[A: EuclideanRing, V](coefficients: Map[Map[V, Int], A]): MultivariablePolynomial[Fraction[A], V] = MultivariablePolynomial(coefficients.mapValues(a => (a: Fraction[A])))
  implicit def liftCoefficientToPolynomials[A, V](coefficients: Map[Map[V, Int], A]): MultivariablePolynomial[Polynomial[A], V] = coefficients.mapValues(a => (a: Polynomial[A]))
  implicit def liftCoefficientToRationalFunctions[A: EuclideanRing, V](coefficients: Map[Map[V, Int], A]): MultivariablePolynomial[Polynomial[Fraction[A]], V] = coefficients.mapValues(a => (a: Polynomial[Fraction[A]]))
  implicit def constant[A: Ring, V](a: A): MultivariablePolynomial[A, V] = {
    if (a == implicitly[Ring[A]].zero) {
      MultivariablePolynomial(Map())
    } else {
      MultivariablePolynomial(Map(Map.empty -> a))
    }
  }
  implicit def constantFraction[A: EuclideanRing, V](a: A): MultivariablePolynomial[Fraction[A], V] = MultivariablePolynomial(Map(Map.empty -> (a: Fraction[A])))

  //  implicit def constantRationalFunction[A: Field, V: Ordering](a: A): MultivariableRationalFunction[A, V] = MultivariablePolynomial[A, V](Map(Map.empty -> a))
  //  implicit def constantToFractionRationalFuncation[A: EuclideanRing, V: Ordering](a: A): MultivariableRationalFunction[Fraction[A], V] = constantRationalFunction(a)
  //  implicit def liftToRationalFunction[A: Field, V: Ordering](coefficients: Map[Map[V, Int], A]): MultivariableRationalFunction[A, V] = lift(coefficients)
  //  implicit def liftCoefficientsToFractions[A: EuclideanRing, V: Ordering](coefficients: Map[Map[V, Int], A]): MultivariableRationalFunction[Fraction[A], V] = liftToRationalFunction(coefficients.mapValues(a => (a: Fraction[A])))

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
//  implicit def multivariablePolynomialAlgebraAsEuclideanRing[A: EuclideanRing, V: Ordering]: EuclideanRing[MultivariablePolynomial[A, V]] = implicitly[MultivariablePolynomialAlgebraOverEuclideanRing[A, V]]
//  implicit def multivariablePolynomialAlgebraAsOrderedEuclideanRing[A: OrderedEuclideanRing, V: Ordering]: OrderedEuclideanRing[MultivariablePolynomial[A, V]] = implicitly[MultivariablePolynomialAlgebraOverOrderedEuclideanRing[A, V]]
}
