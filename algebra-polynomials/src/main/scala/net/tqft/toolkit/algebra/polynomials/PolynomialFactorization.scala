package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

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
    factorSquareAndContentFree(squareFreeFactorization(x)).map(f => f -> ???).toMap
  }
  def factorSquareAndContentFree(x: Polynomial[A]): Seq[Polynomial[A]]
}

object ZassenhausFactoring {
  implicit class FactorizablePolynomials[A:IntegerModel](polynomials: PolynomialsOverIntegerModel[A]) extends PolynomialFactorization[A] {
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
