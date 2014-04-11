package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

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

  def constantTerm(p: MultivariablePolynomial[A, V]): A = {
    p.coefficients.get(Map()).getOrElse(ring.zero)
  }
  def highestMonomial(p: MultivariablePolynomial[A, V]) = {
    import net.tqft.toolkit.arithmetic.MinMax._
    p.coefficients.keySet.minOption(monomialOrdering)
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

    result
  }
  def fromUnivariatePolynomialInVariable(v: V)(p: Polynomial[MultivariablePolynomial[A, V]]): MultivariablePolynomial[A, V] = {
    sum(p.coefficients.map({
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

trait MultivariablePolynomialAlgebra[A, V] extends Ring[MultivariablePolynomial[A, V]] with MultivariablePolynomialAlgebraOverRig[A, V] {
  implicit override def ring: Ring[A]

  override protected val implementation = new Ring.RingMap[Map[V, Int], A] {
    override def keys = implicitly[AdditiveGroup[Map[V, Int]]]
    override def coefficients = implicitly[Module[A, A]]
    override def multiplicativeCoefficients = implicitly[Ring[A]]
  }
  override def negate(p: MultivariablePolynomial[A, V]) = implementation.negate(p.coefficients)
}

object MultivariablePolynomialAlgebra {
  implicit def over[A: Ring, V: Ordering]: MultivariablePolynomialAlgebra[A, V] = new MultivariablePolynomialAlgebra[A, V] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
    override val variableOrdering = implicitly[Ordering[V]]
    override val ring = implicitly[Ring[A]]
  }
}

trait MultivariablePolynomialAlgebraOverEuclideanRing[A, V] extends MultivariablePolynomialAlgebra[A, V] with GCDRing[MultivariablePolynomial[A, V]] {
  override implicit def ring: EuclideanRing[A]

  override def gcd(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    import net.tqft.toolkit.arithmetic.MinMax._
    variablesByMaximumDegree(x).headOption.orElse(variablesByMaximumDegree(y).headOption).map(_._2.head) match {
      case None => {
        if (x.coefficients.isEmpty && y.coefficients.isEmpty) {
          one
        } else if (x.coefficients.isEmpty || y.coefficients.isEmpty) {
          x.coefficients ++ y.coefficients
        } else {
          ring.gcd(x.coefficients(Map()), y.coefficients(Map()))
        }
      }
      case Some(v) => {
        implicit val polynomials = this
        val rationalFunctions = implicitly[Ring[MultivariableRationalFunction[A, V]]]
        val univariatePolynomialsInMultivariablePolynomials = implicitly[PolynomialsOverGCDRing[MultivariablePolynomial[A, V]]]
        val univariatePolynomialsInMultivariableRationalFunctions = implicitly[PolynomialsOverFieldOfFractions[MultivariablePolynomial[A, V]]]

        val xp = asUnivariatePolynomialInVariable(v)(x)
        val yp = asUnivariatePolynomialInVariable(v)(y)

        def verifyResult(r: MultivariablePolynomial[A, V]): Boolean = {
          multiply(r, exactQuotient(x, r)) == x &&
          multiply(r, exactQuotient(y, r)) == y
        }
        
        if ((variables(x) ++ variables(y)).size == 1) {
          // actually, there are no more variables in the coefficient functions, switch to univariate gcd
        	val xo: Polynomial[A] = xp.coefficients.mapValues(p => constantTerm(p))
        	val yo: Polynomial[A] = yp.coefficients.mapValues(p => constantTerm(p))
        	
        	val univariatePolynomials = implicitly[PolynomialsOverGCDRing[A]]
        	val univariateGCD = univariatePolynomials.gcd(xo, yo)
        	fromUnivariatePolynomialInVariable(v)(univariateGCD.coefficients.mapValues(a => MultivariablePolynomial.constant[A, V](a)))//.ensuring(verifyResult _)
        } else {
          val xc = univariatePolynomialsInMultivariablePolynomials.content(xp)
          val yc = univariatePolynomialsInMultivariablePolynomials.content(yp)

          val xv = xp.coefficients.mapValues(p => Fraction.whole(p)(polynomials))
          val yv = yp.coefficients.mapValues(p => Fraction.whole(p)(polynomials))

          val gcdOverRationalFunctions = univariatePolynomialsInMultivariableRationalFunctions.gcd(xv, yv)
          val primitivePart = univariatePolynomialsInMultivariableRationalFunctions.primitivePartOverFractions(gcdOverRationalFunctions)

          val contentGCD = gcd(xc, yc)

          multiply(
            contentGCD,
            fromUnivariatePolynomialInVariable(v)(primitivePart))//.ensuring(verifyResult _)
        }
      }
    }
  }

  override def exactQuotientOption(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]): Option[MultivariablePolynomial[A, V]] = {
    import net.tqft.toolkit.arithmetic.MinMax._
    variablesByMaximumDegree(x).headOption.orElse(variablesByMaximumDegree(y).headOption).map(_._2.head) match {
      case None => {
        if (y.coefficients.isEmpty) {
          throw new ArithmeticException
        } else {
          if (x.coefficients.isEmpty) {
            Some(zero)
          } else {
            ring.exactQuotientOption(x.coefficients(Map()), y.coefficients(Map())).map({ x => x })
          }
        }
      }
      case Some(v) => {
        // it is not clear to me that this is a good method
        implicit val polynomials = this
        val univariatePolynomialsInMultivariableRationalFunctions = implicitly[PolynomialsOverFieldOfFractions[MultivariablePolynomial[A, V]]]

        val xp = asUnivariatePolynomialInVariable(v)(x).coefficients.mapValues(p => Fraction.whole(p)(polynomials))
        val yp = asUnivariatePolynomialInVariable(v)(y).coefficients.mapValues(p => Fraction.whole(p)(polynomials))

        univariatePolynomialsInMultivariableRationalFunctions.exactQuotientOption(xp, yp).flatMap({ p =>
          if (p.coefficients.values.forall(_.denominator == one)) {
            Some(fromUnivariatePolynomialInVariable(v)(p.coefficients.mapValues(_.numerator)))
          } else {
            None
          }
        })
      }
    }
  }

}

object MultivariablePolynomialAlgebraOverEuclideanRing {
  implicit def over[A: EuclideanRing, V: Ordering]: MultivariablePolynomialAlgebraOverEuclideanRing[A, V] = new MultivariablePolynomialAlgebraOverEuclideanRing[A, V] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
    override val variableOrdering = implicitly[Ordering[V]]
    override val ring = implicitly[EuclideanRing[A]]
  }
}
//
//object MultivariablePolynomialAlgebraOverOrderedEuclideanRing {
//  implicit def over[A: OrderedEuclideanRing, V: Ordering]: MultivariablePolynomialAlgebraOverOrderedEuclideanRing[A, V] = new MultivariablePolynomialAlgebraOverOrderedEuclideanRing[A, V] {
//    override val variableOrdering = implicitly[Ordering[V]]
//    override val ring = implicitly[OrderedEuclideanRing[A]]
//  }
//}