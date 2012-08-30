package net.tqft.toolkit.algebra

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

trait ImplicitEuclideanDomains extends ImplicitRings {
  override implicit val Integers: EuclideanDomain[Int] = Gadgets.Integers
  override implicit val Rationals: EuclideanDomain[Fraction[Int]] = Gadgets.Rationals
  override implicit val BigInts: EuclideanDomain[BigInt] = Gadgets.BigIntegers
  override implicit val BigRationals: EuclideanDomain[Fraction[BigInt]] = Gadgets.BigRationals
  override implicit val RationalPolynomials: EuclideanDomain[Polynomial[Fraction[Int]]] = Gadgets.RationalPolynomials
}

object EuclideanDomain extends ImplicitEuclideanDomains

trait OrderedEuclideanDomain[A] extends EuclideanDomain[A] with Ordering[A] {
  def signum(x: A): A = compare(x, zero) match {
    case 0 => zero
    case x if x < 0 => negate(one)
    case _ => one
  }

  override def gcd(x: A, y: A) = {
    val gcd = super.gcd(x, y)
    multiply(gcd, multiply(signum(gcd), signum(y)))
  }
}