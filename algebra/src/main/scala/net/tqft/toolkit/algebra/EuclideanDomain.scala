package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.polynomials.Polynomial

trait EuclideanDomain[A] extends CommutativeRing[A] {
  def quotientRemainder(x: A, y: A): (A, A)
  def quotient(x: A, y: A): A = quotientRemainder(x, y)._1
  def remainder(x: A, y: A): A = quotientRemainder(x, y)._2

  @scala.annotation.tailrec
  final def euclideanAlgorithm(x: A, y: A): A = {
    if (y == zero) {
      x
    } else {
      euclideanAlgorithm(y, remainder(x, y))
    }
  }

  /**
   *
   * @param x
   * @param y
   * @return (a,b,g) such that a*x + b*y == g, and g is the gcd of x and y
   */
  final def extendedEuclideanAlgorithm(x: A, y: A): (A, A, A) = {
    if (y == zero) {
      (one, zero, x)
    } else {
      val (a1, b1, g) = extendedEuclideanAlgorithm(y, remainder(x, y))
      (b1, subtract(a1, multiply(b1, quotient(x, y))), g)
    }
  }

  def gcd(x: A, y: A): A = euclideanAlgorithm(x, y)
  def gcd(xs: A*): A = {
    xs.size match {
      case 0 => one
      case 1 => xs.head
      case _ => gcd((gcd(xs(0), xs(1)) +: xs.drop(2)): _*)
    }
  }
  def lcm(x: A, y: A): A = quotient(multiply(x, y), gcd(x, y))
  def lcm(xs: A*): A = {
    xs.size match {
      case 0 => one
      case 1 => xs.head
      case _ => lcm((lcm(xs(0), xs(1)) +: xs.drop(2)): _*)
    }
  }

  def digits(x: A, base: A = fromInt(10)): Seq[A] = {
    if (x == zero) {
      Seq.empty
    } else {
      quotientRemainder(x, base) match {
        case (q, r) => digits(q, base) :+ r
      }
    }
  }
}

trait ImplicitEuclideanDomains {
  implicit def forgetField[A: Field]: EuclideanDomain[A] = implicitly[Field[A]]
  // we can't forget from OrderedEuclideanDomains, because Field[Fraction[Int]] could then be found two different ways.
  implicit def forgetIntegerModel[A: IntegerModel]: OrderedEuclideanDomain[A] = implicitly[IntegerModel[A]]
  implicit def polynomialAlgebra[A: polynomials.PolynomialAlgebraOverField]: EuclideanDomain[polynomials.Polynomial[A]] = implicitly[polynomials.PolynomialAlgebraOverField[A]]
}

object EuclideanDomain extends ImplicitEuclideanDomains

