package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

trait PolynomialAlgebraOverGCDRing[A, P] extends PolynomialAlgebra[A, P] with GCDRing[P] {
  override implicit def ring: GCDRing[A]

  def scalarExactQuotient(p: P, a: A): P

  def content(p: P): A = {
    ring.gcd(toMap(p).values.toSeq: _*)
  }
  def primitivePart(p: P): P = {
    val c = content(p)
    scalarExactQuotient(p, c)
  }

  def pseudoQuotientRemainder(f: P, g: P): (P, P) = {
    val df = maximumDegree(f).getOrElse(0)
    val dg = maximumDegree(g).getOrElse(0)
    val result = if (df == 0 && dg == 0) {
      (f, zero)
    } else {
      val d = if (df >= dg) df - dg + 1 else 0
      if (df < dg) {
        (zero, f)
      } else {
        val b = leadingCoefficient(g).get
        //        println("b = " + b)

        val a = monomial(df - dg, leadingCoefficient(f).get)
        //        println("a = " + a)

        val `bf-ag` = subtract(scalarMultiply(b, f), multiply(a, g))
        //        println("bf-ag = " + `bf-ag`)

        val e = {
          val d_ = maximumDegree(`bf-ag`).getOrElse(0)
          require(d_ < df)
          if (d_ >= dg) {
            d_ - dg + 1
          } else {
            0
          }
        }
        val (s, r0) = pseudoQuotientRemainder(`bf-ag`, g)
        //        println("s = " + s)
        //        println("r0 = " + r0)
        val `b^(d-e-1)` = ring.power(b, d - e - 1)
        val r = scalarMultiply(`b^(d-e-1)`, r0)
        //        println("r = " + r0)
        (add(scalarMultiply(`b^(d-e-1)`, s), scalarMultiply(ring.power(b, d - 1), a)), r)

        //b^e(bf-ag) == gs + r
        //b^(e+1) f == g(s+b^e a) + r
        //b^d f == g b^(d-e-1) (s + b^e a) + b^(d-e-1) r
        //      == g (b^(d-e-1) s + b^(d-1) a) + b^(d-e-1) r

      }
    }
    //    require({
    //      val q = result._1
    //      scalarMultiply(ring.power(b, d), f) == add(multiply(g, q), r)
    //    })
    //    println("result._1 = " + result._1)
    //    println("result._2 = " + result._2)

    result

  }

  def pseudoRemainder(f: P, g: P): P = pseudoQuotientRemainder(f, g)._2

  def subresultantSequence(f0: P, f1: P): Stream[P] = {
    require(maximumDegree(f0).getOrElse(0) >= maximumDegree(f1).getOrElse(0))
    lazy val f: Stream[P] = f0 #:: f1 #:: (Stream.from(2).map({ i =>
      val beta = ring.multiply(
        (if (n(i - 2) - n(i - 1) + 1 % 2 == 0) ring.fromInt(1) else ring.fromInt(-1)),
        a(i - 2),
        ring.power(c(i - 2), n(i - 2) - n(i - 1)))
      scalarExactQuotient(pseudoRemainder(f(i - 2), f(i - 1)), beta)
    }))
    lazy val n: Stream[Int] = f.map(p => maximumDegree(p).get)
    lazy val a: Stream[A] = ring.fromInt(1) #:: f.tail.map(p => leadingCoefficient(p).get)
    lazy val c: Stream[A] = ring.fromInt(1) #:: Stream.from(1).map({ i =>
      val k = n(i) - n(i - 1) + 1
      if (k >= 0) {
        ring.multiply(
          ring.power(a(i), n(i - 1) - n(i)),
          ring.power(c(i - 1), k))
      } else {
        ring.exactQuotient(
          ring.power(a(i), n(i - 1) - n(i)),
          ring.power(c(i - 1), -k))
      }
    })
    f
  }

  def subresultant_gcd(f0: P, f1: P): P = {
    if (maximumDegree(f0).getOrElse(0) < maximumDegree(f1).getOrElse(0)) {
      subresultant_gcd(f1, f0)
    } else {
      if (zero_?(f0)) {
        f1
      } else {
        val seq = subresultantSequence(f0, f1)
        seq.takeWhile(p => maximumDegree(p).nonEmpty).last
      }
    }
  }

  override def gcd(x: P, y: P): P = {
    if (x == one || y == one) {
      one
    } else {
      val xc = content(x)
      val yc = content(y)
      val contentGCD = ring.gcd(xc, yc)

      val primitive = primitivePart(subresultant_gcd(x, y))

      scalarMultiply(
        contentGCD,
        primitive)
    }
  }
  override def exactQuotientOption(x: P, y: P): Option[P] = {
    //    println(s"exactQuotientOption($x, $y)")

    (maximumDegree(x), maximumDegree(y)) match {
      case (_, None) => throw new ArithmeticException
      case (None, Some(dy)) => Some(zero)
      case (Some(dx), Some(dy)) => {
        if (dy > dx) {
          None
        } else {
          val ax = leadingCoefficient(x).get
          val ay = leadingCoefficient(y).get

          require(!ring.zero_?(ax) )
          require(!ring.zero_?(ay))

          ring.exactQuotientOption(ax, ay) match {
            case None => None
            case Some(q) => {
              val quotientLeadingTerm = monomial(dx - dy, q)
              val difference = subtract(x, multiply(quotientLeadingTerm, y))
              require(maximumDegree(difference).isEmpty || maximumDegree(difference).get < maximumDegree(x).get)
              exactQuotientOption(difference, y).map({ restOfQuotient => add(quotientLeadingTerm, restOfQuotient) })
            }
          }
        }
      }
    }
  }
  def multiplicityFactorization(p: P): Stream[P] = {
    maximumDegree(p) match {
      case None | Some(0) => Stream(p)
      case _ => gcd(p, formalDerivative(p)) match {
        case q if maximumDegree(q).get == 0 => Stream(q)
        case q => exactQuotient(p, q) #:: multiplicityFactorization(q)
      }
    }
  }

  def squareFreeFactorization(p: P): P = multiplicityFactorization(p).head

}

object PolynomialAlgebraOverGCDRing {
  trait PolynomialAlgebraOverGCDRingForMaps[A] extends PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverGCDRing[A, Map[Int, A]] {
    override def ring: GCDRing[A]
    override def scalarExactQuotient(p: Map[Int, A], a: A) = p.mapValues(x => ring.exactQuotient(x, a))
  }

  implicit def forMaps[A: GCDRing]: PolynomialAlgebraOverGCDRing[A, Map[Int, A]] = new PolynomialAlgebraOverGCDRingForMaps[A] {
    override def ring = implicitly[GCDRing[A]]
  }
  implicit def over[A: GCDRing]: PolynomialAlgebraOverGCDRing[A, Polynomial[A]] = PolynomialsOverGCDRing.over[A]
}

abstract class PolynomialsOverGCDRing[A: GCDRing] extends Polynomials[A] with PolynomialAlgebraOverGCDRing[A, Polynomial[A]] {
  override def scalarExactQuotient(p: Polynomial[A], a: A) = p.mapValues(x => ring.exactQuotient(x, a))
  override def toString = s"PolynomialsOverGCDRing.over($ring)"
}

object PolynomialsOverGCDRing {
  implicit def over[A: GCDRing]: PolynomialsOverGCDRing[A] = new PolynomialsOverGCDRing[A] {
    override def ring = implicitly[GCDRing[A]]
  }
}

