package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

trait MultivariablePolynomialAlgebraOverRig[A, V] extends Rig[MultivariablePolynomial[A, V]] with ModuleOverRig[A, MultivariablePolynomial[A, V]] {
  implicit def ring: Rig[A]

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

  val monomialOrdering: Ordering[Map[V, Int]]

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

  def highestMonomial(p: MultivariablePolynomial[A, V]) = {
    import net.tqft.toolkit.arithmetic.MinMax._
    p.coefficients.keySet.minOption(monomialOrdering)
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

}

trait MultivariablePolynomialAlgebra[A, V] extends Ring[MultivariablePolynomial[A, V]] with MultivariablePolynomialAlgebraOverRig[A, V] {
  implicit override def ring: Ring[A]

  override protected val implementation = new Ring.RingMap[Map[V, Int], A] {
    override def keys = implicitly[AdditiveGroup[Map[V, Int]]]
    override def coefficients = implicitly[Module[A, A]]
    override def multiplicativeCoefficients = implicitly[Ring[A]]
  }
  override def negate(p: MultivariablePolynomial[A, V]) = implementation.negate(p.coefficients)
}

object MultivariablePolynomialAlgebraOverRig {
  trait LexicographicOrdering[A, V] { self: MultivariablePolynomialAlgebraOverRig[A, V] =>
    implicit def variableOrdering: Ordering[V]

    override lazy val monomialOrdering = {
      import net.tqft.toolkit.orderings.LexicographicOrdering._
      implicitly[Ordering[Map[V, Int]]]
    }
  }

  implicit def overRig[A: Rig, V: Ordering]: MultivariablePolynomialAlgebraOverRig[A, V] = new MultivariablePolynomialAlgebraOverRig[A, V] with LexicographicOrdering[A, V] {
    override val variableOrdering = implicitly[Ordering[V]]
    override val ring = implicitly[Rig[A]]
  }
}

object MultivariablePolynomialAlgebra {
  implicit def over[A: Ring, V: Ordering]: MultivariablePolynomialAlgebra[A, V] = new MultivariablePolynomialAlgebra[A, V] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
    override val variableOrdering = implicitly[Ordering[V]]
    override val ring = implicitly[Ring[A]]
  }
}

trait MultivariablePolynomialAlgebraOverEuclideanRing[A, V] extends MultivariablePolynomialAlgebra[A, V] {
  override def ring: EuclideanRing[A]

  def divideByCoefficientGCD(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    val gcd = ring.gcd(p.coefficients.values.toSeq: _*)
    if (gcd == ring.one) {
      p
    } else {
      MultivariablePolynomial(p.coefficients.mapValues(v => ring.quotient(v, gcd)))
    }
  }
}

object MultivariablePolynomialAlgebraOverEuclideanRing {
  implicit def over[A: EuclideanRing, V: Ordering]: MultivariablePolynomialAlgebraOverEuclideanRing[A, V] = new MultivariablePolynomialAlgebraOverEuclideanRing[A, V] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
    override val variableOrdering = implicitly[Ordering[V]]
    override val ring = implicitly[EuclideanRing[A]]
  }
}

//trait MultivariablePolynomialAlgebraOverField[A, V] extends MultivariablePolynomialAlgebra[A, V] with EuclideanRing[MultivariablePolynomial[A, V]] {
//  override def ring: Field[A]
//  override def quotientRemainder(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]) = {
//    println("quotientRemainder(")
//    println("  " + x + ",")
//    println("  " + y)
//    println(")")
//
//    highestMonomial(y) match {
//      case None => throw new ArithmeticException
//      case Some(lm) => {
//        val quotientMonomials = x.coefficients.keys.map(m => implementation.keys.subtract(m, lm)).filter(_.values.forall(_ >= 0))
//        import net.tqft.toolkit.orderings.LexicographicOrdering._
//        import net.tqft.toolkit.arithmetic.MinMax._
//        quotientMonomials.maxOption(monomialOrdering) match {
//          case None => (zero, x)
//          case Some(m) => {
//            val quotientLeadingTerm = monomial(m, x.coefficients(implementation.keys.add(m, lm)))
//            val difference = add(x, negate(multiply(quotientLeadingTerm, y)))
//              val (restOfQuotient, remainder) = quotientRemainder(difference, y)
//              (add(quotientLeadingTerm, restOfQuotient), remainder)
//          }
//        }
//      }
//    }
//  }
//}
//
//object MultivariablePolynomialAlgebraOverField {
//  implicit def over[A: Field, V: Ordering]: MultivariablePolynomialAlgebraOverField[A, V] = new MultivariablePolynomialAlgebraOverField[A, V] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
//    override val variableOrdering = implicitly[Ordering[V]]
//    override val ring = implicitly[Field[A]]
//  }
//}
//
//trait MultivariablePolynomialAlgebraOverOrderedField[A, V] extends MultivariablePolynomialAlgebraOverField[A, V] with OrderedEuclideanRing[MultivariablePolynomial[A, V]]
//
//object MultivariablePolynomialAlgebraOverOrderedField {
//  implicit def over[A: OrderedField, V: Ordering]: MultivariablePolynomialAlgebraOverOrderedField[A, V] = new MultivariablePolynomialAlgebraOverOrderedField[A, V] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
//    override val variableOrdering = implicitly[Ordering[V]]
//    override val ring = implicitly[OrderedField[A]]
//    override def compare(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]): Int = {
//      import net.tqft.toolkit.orderings.LexicographicOrdering._
//      implicitly[Ordering[Map[Map[V, Int], A]]].compare(x.coefficients, y.coefficients)
//    }
//  }
//}