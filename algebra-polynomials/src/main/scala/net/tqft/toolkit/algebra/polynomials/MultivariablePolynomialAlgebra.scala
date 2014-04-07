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
    (variables(x) ++ variables(y)).maxByOption(v => Map(v -> 1))(monomialOrdering) match {
      case None => {
        if (x.coefficients.isEmpty && y.coefficients.isEmpty) {
          one
        } else if(x.coefficients.isEmpty || y.coefficients.isEmpty) {
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

        val xc = univariatePolynomialsInMultivariablePolynomials.content(xp)
        val yc = univariatePolynomialsInMultivariablePolynomials.content(yp)

        val xv = xp.coefficients.mapValues(p => Fraction.whole(p)(polynomials))
        val yv = yp.coefficients.mapValues(p => Fraction.whole(p)(polynomials))

        val gcdOverRationalFunctions = univariatePolynomialsInMultivariableRationalFunctions.gcd(xv, yv)
        val primitivePart = univariatePolynomialsInMultivariableRationalFunctions.primitivePart(gcdOverRationalFunctions)

        val contentGCD = gcd(xc, yc)
        
        val result = multiply(
          contentGCD,
          fromUnivariatePolynomialInVariable(v)(primitivePart))

//        require(multiply(result, exactQuotient(x, result)) == x)
//        require(multiply(result, exactQuotient(y, result)) == y)

        result
      }
    }
  }

  override def exactQuotientOption(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]): Option[MultivariablePolynomial[A, V]] = {
    import net.tqft.toolkit.arithmetic.MinMax._
    (variables(x) ++ variables(y)).maxByOption(v => Map(v -> 1))(monomialOrdering) match {
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

  //  private var stackDepth = 0
  //  override def quotientRemainder(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]): (MultivariablePolynomial[A, V], MultivariablePolynomial[A, V]) = {
  //    println("quotientRemainder(")
  //    println("  " + x + ",")
  //    println("  " + y)
  //    println(")")
  //
  //    stackDepth = stackDepth + 1
  //    require(stackDepth < 40)
  //    import net.tqft.toolkit.arithmetic.MinMax._
  //    variables(x).maxByOption(v => Map(v -> 1))(monomialOrdering) match {
  //      case None => {
  //        stackDepth = stackDepth - 1
  //        // x is just a constant
  //        if (variables(y).isEmpty) {
  //          val (aq, ar) = ring.quotientRemainder(x.constantTerm, y.constantTerm)
  //          (aq, ar)
  //        } else {
  //          (zero, x)
  //        }
  //      }
  //      case Some(v) => {
  //        val dx = maximumDegreeOfVariable(v)(x).get
  //        val ody = maximumDegreeOfVariable(v)(y)
  //        ody match {
  //          case None => throw new ArithmeticException
  //          case Some(dy) => {
  //            if (dx < dy) {
  //              stackDepth = stackDepth - 1
  //              (zero, x)
  //            } else {
  //              val ax = coefficientOfOneVariable(v, dx)(x)
  //              val ay = coefficientOfOneVariable(v, dy)(y)
  //              val q = quotient(ax, ay) // ax = q ay + r
  //              if (q != zero) {
  //                val q1 = multiply(q, monomial(Map(v -> (dx - dy))))
  //                val difference = add(x, negate(multiply(q1, y)))
  //                val (q2, r) = quotientRemainder(difference, y)
  //                stackDepth = stackDepth - 1
  //                (add(q1, q2), r).ensuring(p => x == add(multiply(p._1, y), p._2))
  //              } else {
  //                stackDepth = stackDepth - 1
  //                (zero, x)
  //              }
  //            }
  //          }
  //        }
  //      }
  //    }
  //  }
}

object MultivariablePolynomialAlgebraOverEuclideanRing {
  implicit def over[A: EuclideanRing, V: Ordering]: MultivariablePolynomialAlgebraOverEuclideanRing[A, V] = new MultivariablePolynomialAlgebraOverEuclideanRing[A, V] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
    override val variableOrdering = implicitly[Ordering[V]]
    override val ring = implicitly[EuclideanRing[A]]
  }
}

//
//trait MultivariablePolynomialAlgebraOverOrderedEuclideanRing[A, V] extends MultivariablePolynomialAlgebraOverEuclideanRing[A, V] with OrderedEuclideanRing[MultivariablePolynomial[A, V]] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
//  override implicit def ring: OrderedEuclideanRing[A]
//  override def compare(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]) = {
//    import net.tqft.toolkit.orderings.LexicographicOrdering
//    LexicographicOrdering.mapOrdering[Map[V, Int], A](monomialOrdering, ring).compare(x.coefficients, y.coefficients)
//  }
//}
//
//object MultivariablePolynomialAlgebraOverOrderedEuclideanRing {
//  implicit def over[A: OrderedEuclideanRing, V: Ordering]: MultivariablePolynomialAlgebraOverOrderedEuclideanRing[A, V] = new MultivariablePolynomialAlgebraOverOrderedEuclideanRing[A, V] {
//    override val variableOrdering = implicitly[Ordering[V]]
//    override val ring = implicitly[OrderedEuclideanRing[A]]
//  }
//}