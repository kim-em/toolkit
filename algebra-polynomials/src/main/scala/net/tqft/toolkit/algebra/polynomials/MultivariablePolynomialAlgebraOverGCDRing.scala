package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

trait MultivariablePolynomialAlgebraOverGCDRing[A, V] extends MultivariablePolynomialAlgebra[A, V] with GCDRing[MultivariablePolynomial[A, V]] {
  override implicit def ring: GCDRing[A]

  override def toString = s"MultivariablePolynomialAlgebraOverGCDRing.over($ring, $variableOrdering)"
  
  def scalarExactQuotient(p: MultivariablePolynomial[A, V], a: A): MultivariablePolynomial[A, V] = p.mapValues(x => ring.exactQuotient(x, a))
  def content(p: MultivariablePolynomial[A, V]): A = {
    ring.gcd(p.coefficients.values.toSeq: _*)
  }
  def primitivePart(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
    val c = content(p)
    scalarExactQuotient(p, c)
  }

  override def gcd(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
//    println(s"gcd($x, $y)")
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
          val xo: Polynomial[A] = xp.mapValues(p => constantTerm(p))
          val yo: Polynomial[A] = yp.mapValues(p => constantTerm(p))

          val univariatePolynomials = implicitly[PolynomialsOverGCDRing[A]]
          val univariateGCD = univariatePolynomials.gcd(xo, yo)
          fromUnivariatePolynomialInVariable(v)(univariateGCD.mapValues(a => MultivariablePolynomial.constant[A, V](a))) //.ensuring(verifyResult _)
        } else {
          fromUnivariatePolynomialInVariable(v)(univariatePolynomialsInMultivariablePolynomials.gcd(xp, yp)) //.ensuring(verifyResult _)
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
        implicit val polynomials = this

        val univariatePolynomialsInMultivariablePolynomials = implicitly[PolynomialsOverGCDRing[MultivariablePolynomial[A, V]]]

        univariatePolynomialsInMultivariablePolynomials.exactQuotientOption(
          asUnivariatePolynomialInVariable(v)(x),
          asUnivariatePolynomialInVariable(v)(y)).map(fromUnivariatePolynomialInVariable(v))
      }
    }
  }


}

object MultivariablePolynomialAlgebraOverGCDRing {
  implicit def over[A: GCDRing, V: Ordering]: MultivariablePolynomialAlgebraOverGCDRing[A, V] = new MultivariablePolynomialAlgebraOverGCDRing[A, V] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
    override val variableOrdering = implicitly[Ordering[V]]
    override val ring = implicitly[GCDRing[A]]
  }
}

