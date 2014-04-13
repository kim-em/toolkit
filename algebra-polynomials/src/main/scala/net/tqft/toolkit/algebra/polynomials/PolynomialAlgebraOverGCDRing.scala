package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.GCDRing
import net.tqft.toolkit.algebra.Fraction

trait PolynomialAlgebraOverGCDRing[A, P] extends PolynomialAlgebra[A, P] with GCDRing[P] {
  override implicit def ring: GCDRing[A]

  def content(p: P): A = {
    ring.gcd(toMap(p).values.toSeq: _*)
  }
  def primitivePart(p: P): P = {
    val c = content(p)
    fromMap(toMap(p).mapValues(x => ring.exactQuotient(x, c)))
  }

  def pseudoQuotientRemainder(f: P, g: P): (P, P) = {
    println("f = " + f)
    println("g = " + g)
    val df = maximumDegree(f).getOrElse(0)
    val dg = maximumDegree(g).getOrElse(0)
    if (df == 0 && dg == 0) {
      ???
    } else {
      val d = if (df >= dg) df - dg + 1 else 0
      if (df < dg) {
        (zero, f)
      } else {
        val b = leadingCoefficient(g).get
        val a = monomial(df - dg, leadingCoefficient(f).get)
        val `bf-ag` = subtract(scalarMultiply(b, f), multiply(a, g))
        require(maximumDegree(`bf-ag`).getOrElse(0) < df)

        val e = {
          val d_ = maximumDegree(`bf-ag`).getOrElse(0)
          if (d_ >= dg) {
            d_ - dg + 1
          } else {
            0
          }
        }
        require(e >= 0)
        val (s, r0) = pseudoQuotientRemainder(`bf-ag`, g)
        val `b^(d-e-1)` = ring.power(b, d - e - 1)
        val r = scalarMultiply(`b^(d-e-1)`, r0)
        val result = (add(scalarMultiply(`b^(d-e-1)`, s), scalarMultiply(ring.power(b, d - 1), a)), r)

        //b^e(bf-ag) == gs + r
        //b^(e+1) f == g(s+b^e a) + r
        //b^d f == g b^(d-e-1) (s + b^e a) + b^(d-e-1) r
        //      == g (b^(d-e-1) s + b^(d-1) a) + b^(d-e-1) r

        require({
          val q = result._1
          scalarMultiply(ring.power(b, d), f) == add(multiply(g, q), r)
        })

        result
      }
    }
  }

  def pseudoRemainder(f: P, g: P): P = pseudoQuotientRemainder(f, g)._2

  def subresultantSequence(f0: P, f1: P): Stream[P] = {
    require(maximumDegree(f0).getOrElse(0) >= maximumDegree(f1).getOrElse(0))
    lazy val f: Stream[P] = f0 #:: f1 #:: (Stream.from(2).map({ i =>
      val beta = ring.multiply(
        (if (n(i - 2) - n(i - 1) + 1 % 2 == 0) ring.fromInt(1) else ring.fromInt(-1)),
        a(i - 2),
        ring.power(c(i - 2), n(i - 2) - n(i - 1)))
      fromMap(Map() ++ toMap(pseudoRemainder(f(i - 2), f(i - 1))).mapValues({ x =>
        ring.exactQuotient(x, beta)
      }))
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

  def subresultant_gcd(f0: P, f1: P) = subresultantSequence(f0, f1).takeWhile(p => maximumDegree(p).nonEmpty).last

  // subresultant_gcd gives (hopefully efficiently) the gcd after extending the coefficients to the field of fractions.
  //  def subresultant_gcd(a: P, b: P): P = {
  //    val gcdRing = implicitly[GCDRing[A]]
  //
  //    if (toMap(a).isEmpty && toMap(b).isEmpty) {
  //      one
  //    } else if (toMap(a).isEmpty) {
  //      b
  //    } else if (toMap(b).isEmpty) {
  //      a
  //    } else {
  //
  //      def _remainder(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {
  //        // TODO can we do this better?
  //        // not unless we have a EuclideanRing[A]?
  //        val rationalPolynomials = implicitly[PolynomialsOverEuclideanRing[Fraction[A]]]
  //        val rationalRemainder = rationalPolynomials.remainder(x.mapValues(a => (a: Fraction[A])), y.mapValues(a => (a: Fraction[A])))
  //        rationalRemainder.mapValues(f => f.ensuring(_.denominator == gcdRing.one).numerator)
  //      }
  //
  //      var r0 = a
  //      var r1 = b
  //      var d = maximumDegree(a).get - maximumDegree(b).get
  //      if (d < 0) {
  //        d = -d
  //        r0 = b
  //        r1 = a
  //      }
  //      var gamma = leadingCoefficient(r1).get
  //      var oldgamma = gamma
  //      var beta = gcdRing.fromInt(if (d % 2 == 0) -1 else 1)
  //      var psi = gcdRing.fromInt(-1)
  //      var done = r1 == zero
  //      while (!done) {
  //        println("r0 = " + r0)
  //        println("r1 = " + r1)
  //        println("d = " + d)
  //        println("gamma = " + gamma)
  //        println("beta = " + beta)
  //        println("psi = " + psi)
  //        val oldr1 = r1
  //        val `gamma^(d+1) r0` = scalarMultiply(gcdRing.power(gamma, d + 1), r0)
  //        println("gamma^(d+1) r0 = " + `gamma^(d+1) r0`)
  //        val remainder = _remainder(toMap(`gamma^(d+1) r0`), toMap(r1))
  //        println("remainder = " + remainder)
  //        // cheat
  //        beta = ring.gcd(remainder.coefficients.values.toSeq: _*)
  //        r1 = fromMap(remainder.mapValues(c => gcdRing.exactQuotient(c, beta)).coefficients)
  //        r0 = oldr1
  //        done = r1 == zero
  //        if (!done) {
  //          oldgamma = gamma
  //          gamma = leadingCoefficient(r1).get
  //          val `-gamma^d` = gcdRing.power(gcdRing.negate(gamma), d)
  //          val `psi^(d-1)` = if (d == 0) {
  //            psi
  //          } else {
  //            gcdRing.power(psi, d - 1)
  //          }
  //          //          psi = gcdRing.exactQuotient(`-gamma^d`, `psi^(d-1)`)
  //          //          d = maximumDegree(r0).get - maximumDegree(r1).get
  //          //          beta = gcdRing.negate(gcdRing.multiply(oldgamma, gcdRing.power(psi, d)))
  //
  //        }
  //      }
  //      r0
  //    }
  //  }

  override def gcd(x: P, y: P): P = {
    val xc = content(x)
    val yc = content(y)
    val contentGCD = ring.gcd(xc, yc)

    val primitive = primitivePart(subresultant_gcd(x, y))

    scalarMultiply(
      contentGCD,
      primitive)
  }
  override def exactQuotientOption(x: P, y: P): Option[P] = {
    (maximumDegree(x), maximumDegree(y)) match {
      case (_, None) => throw new ArithmeticException
      case (None, Some(dy)) => Some(zero)
      case (Some(dx), Some(dy)) => {
        if (dy > dx) {
          None
        } else {
          val ax = leadingCoefficient(x).get
          val ay = leadingCoefficient(y).get

          require(ax != ring.zero)
          require(ay != ring.zero)

          ring.exactQuotientOption(ax, ay) match {
            case None => None
            case Some(q) => {
              val quotientLeadingTerm = monomial(dx - dy, q)
              val difference = add(x, negate(multiply(quotientLeadingTerm, y)))
              require(maximumDegree(difference).isEmpty || maximumDegree(difference).get < maximumDegree(x).get)
              exactQuotientOption(difference, y).map({ restOfQuotient => add(quotientLeadingTerm, restOfQuotient) })
            }
          }
        }
      }
    }
  }
}

object PolynomialAlgebraOverGCDRing {
  trait PolynomialAlgebraOverGCDRingForMaps[A] extends PolynomialAlgebra.PolynomialAlgebraForMaps[A] with PolynomialAlgebraOverGCDRing[A, Map[Int, A]] {
    override def ring: GCDRing[A]
  }

  implicit def forMaps[A: GCDRing]: PolynomialAlgebraOverGCDRing[A, Map[Int, A]] = new PolynomialAlgebraOverGCDRingForMaps[A] {
    override def ring = implicitly[GCDRing[A]]
  }
  implicit def over[A: GCDRing]: PolynomialAlgebraOverGCDRing[A, Polynomial[A]] = PolynomialsOverGCDRing.over[A]
}

abstract class PolynomialsOverGCDRing[A: GCDRing] extends Polynomials[A] with PolynomialAlgebraOverGCDRing[A, Polynomial[A]]
object PolynomialsOverGCDRing {
  implicit def over[A: GCDRing]: PolynomialsOverGCDRing[A] = new PolynomialsOverGCDRing[A] {
    override def ring = implicitly[GCDRing[A]]
  }
}
