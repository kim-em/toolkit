package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.numberfields.PolynomialQuotientRing

abstract class PolynomialFactorization[A: GCDRing: Factorization] extends Factorization[Polynomial[A]] {
  val polynomials = implicitly[PolynomialsOverGCDRing[A]]
  val factorization = implicitly[Factorization[A]]
  import polynomials._
  override def factor(x: Polynomial[A]): Map[Polynomial[A], Int] = {
    val c = content(x)
    val p = primitivePart(x)
    val contentFactorization: Map[Polynomial[A], Int] = factorization.factor(c).map(p => (constant(p._1), p._2))
    val primitiveFactorization = factorContentFree(p)
    contentFactorization ++ primitiveFactorization
  }
  def factorContentFree(x: Polynomial[A]): Map[Polynomial[A], Int] = {
    // we reduce to the square free case
    def powerDividing(f: Polynomial[A]) = {
      var k = -1
      var p = Some(x)
      while(p.nonEmpty) {
        k = k + 1
        p.flatMap(q => polynomials.exactQuotientOption(q, f))
      }
      k
    }
    
    factorSquareAndContentFree(squareFreeFactorization(x)).map(factor => factor -> powerDividing(factor)).toMap
  }
  def factorSquareAndContentFree(x: Polynomial[A]): Seq[Polynomial[A]]
}

object ZassenhausFactoring {
  implicit class FactorizablePolynomials[A:IntegerModel:Factorization](polynomials: PolynomialsOverIntegerModel[A]) extends PolynomialFactorization[A] {
    import polynomials._
    override def factorSquareAndContentFree(x: Polynomial[A]): Seq[Polynomial[A]] = {
      // pick a prime
      val p: A = ???
      // factor mod p
      // Hensel lift to a factorization mod p^a
      // look for true factors
      ???
    }
  }
}

abstract class PolynomialFactorizationOverField[A: Field] extends Factorization[Polynomial[A]] {
  val polynomials = implicitly[PolynomialsOverField[A]]

  import polynomials._
  override def factor(x: Polynomial[A]): Map[Polynomial[A], Int] = {
    factorSquareFree(squareFreeFactorization(x)).map(f => f -> ???).toMap
  }
  def factorSquareFree(x: Polynomial[A]): Seq[Polynomial[A]]
}

object CantorZassenhausFactoring {
  implicit class FactorizablePolynomials[I: IntegerModel](polynomials: PolynomialsOverFiniteField[I]) extends PolynomialFactorizationOverField[Polynomial[I]]()(polynomials.ring) {
    import polynomials._
    def integers = implicitly[IntegerModel[I]]
    override def factorSquareFree(f: Polynomial[Polynomial[I]]): Seq[Polynomial[Polynomial[I]]] = {
      val quotient = PolynomialQuotientRing(f)(polynomials.ring)
      val b = Iterator.continually(polynomials.randomPolynomial(polynomials.maximumDegree(f).get)).filter(p => p != polynomials.one && p != polynomials.fromInt(-1) && p != polynomials.zero).next
      val a = quotient.power(b, integers.quotient(integers.subtract(polynomials.ring.order, integers.one),integers.fromInt(2)))
      val factors = Seq(polynomials.gcd(f, a), polynomials.gcd(f, polynomials.add(a, polynomials.one)), polynomials.gcd(f, polynomials.subtract(a, polynomials.one))).filter(_ != polynomials.one)
      if(factors.size == 1) {
        factors
      } else {
        factors.flatMap(factorSquareFree)
      }
    }
  }
}

object LLLFactoring {
  implicit class FactorizablePolynomials[A:IntegerModel:Factorization](polynomials: PolynomialsOverIntegerModel[A]) extends PolynomialFactorization[A] {
    import polynomials._
    override def factorSquareAndContentFree(x: Polynomial[A]): Seq[Polynomial[A]] = {
      ???
    }
  }
}