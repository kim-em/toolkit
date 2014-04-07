package net.tqft.toolkit.algebra

trait IntegralRig[A] extends CommutativeRig[A] {
  // nothing to see here; we just add the condition that ab=0 implies a=0 or b=0
  def exactQuotientOption(x: A, y: A): Option[A]
  def exactQuotient(x: A, y: A) = exactQuotientOption(x, y).get
}

trait GCDRig[A] extends IntegralRig[A] {
  def gcd(x: A, y: A): A
  def gcd(xs: A*): A = {
    xs.size match {
      case 0 => one
      case 1 => xs.head
      case _ => gcd((gcd(xs(0), xs(1)) +: xs.drop(2)): _*)
    }
  }
  def lcm(x: A, y: A): A = exactQuotient(multiply(x, y), gcd(x, y))
  def lcm(xs: A*): A = {
    xs.size match {
      case 0 => one
      case 1 => xs.head
      case _ => lcm((lcm(xs(0), xs(1)) +: xs.drop(2)): _*)
    }
  }

}

trait EuclideanRig[A] extends GCDRig[A] {
  def quotientRemainder(x: A, y: A): (A, A)
  def quotient(x: A, y: A): A = quotientRemainder(x, y)._1
  def remainder(x: A, y: A): A = quotientRemainder(x, y)._2

  override def exactQuotientOption(x: A, y: A) = {
    quotientRemainder(x, y) match {
      case (q, r) if r == zero => Some(q)
      case _ => None
    }
  }
  
  @scala.annotation.tailrec
  final def euclideanAlgorithm(x: A, y: A): A = {
    if (y == zero) {
      x
    } else {
      euclideanAlgorithm(y, remainder(x, y))
    }
  }

  override def gcd(x: A, y: A): A = euclideanAlgorithm(x, y)

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

trait GCDRing[A] extends GCDRig[A] with CommutativeRing[A]

trait EuclideanRing[A] extends EuclideanRig[A] with GCDRing[A] with CommutativeRing[A] {
  /**
   *
   * @param x
   * @param y
   * @return (a,b,g) such that a*x + b*y == g, and g is the gcd of x and y
   */
  // FIXME tail recursive
  final def extendedEuclideanAlgorithm(x: A, y: A): (A, A, A) = {
    if (y == zero) {
      (one, zero, x)
    } else {
      val (a1, b1, g) = extendedEuclideanAlgorithm(y, remainder(x, y))
      (b1, subtract(a1, multiply(b1, quotient(x, y))), g)
    }
  }
}

trait GCDRigLowPriorityImplicits {
  implicit def forgetGCDRing[A: GCDRing]: GCDRig[A] = implicitly[GCDRig[A]]  
}

object GCDRig extends GCDRigLowPriorityImplicits {
  implicit def forgetEuclideanRig[A: EuclideanRig]: GCDRig[A] = implicitly[GCDRig[A]]
}
object GCDRing {
  implicit def forgetEuclideanRing[A: EuclideanRing]: GCDRing[A] = implicitly[GCDRing[A]]
}


object EuclideanRig {
  implicit def forgetRing[A: EuclideanRing]: EuclideanRig[A] = implicitly[EuclideanRing[A]]
}

trait EuclideanRingLowPriorityImplicits {
  // implicits inherited from a supertype are given lower priority
  // this lets us forget from OrderedField to EuclideanRing, preferring the route via Field over the route via OrderedEuclideanRing
  implicit def forgetOrderedEuclideanRing[A: OrderedEuclideanRing]: EuclideanRing[A] = implicitly[EuclideanRing[A]]
}

object EuclideanRing extends EuclideanRingLowPriorityImplicits {
  implicit def forgetField[A: Field]: EuclideanRing[A] = implicitly[EuclideanRing[A]]
  implicit def forgetIntegerModel[A: IntegerModel]: EuclideanRing[A] = implicitly[EuclideanRing[A]]
}

