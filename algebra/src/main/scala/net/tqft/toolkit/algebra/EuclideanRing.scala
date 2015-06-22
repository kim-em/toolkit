package net.tqft.toolkit.algebra

trait IntegralRig[@specialized(Int, Long, Float, Double) A] extends CommutativeRig[A] {
  // nothing to see here; we just add the condition that ab=0 implies a=0 or b=0
  def exactQuotientOption(x: A, y: A): Option[A]
  def exactQuotient(x: A, y: A) = exactQuotientOption(x, y).get
}

trait GCDRig[@specialized(Int, Long, Float, Double) A] extends IntegralRig[A] {
  def gcd(x: A, y: A): A
  def gcdWithQuotients(x: A, y: A): (A, A, A) = {
    val g = gcd(x, y)
    (g, exactQuotient(x, g), exactQuotient(y, g))
  }
  def gcd(xs: A*): A = {
    xs.size match {
      case 0 => zero
      case 1 => xs.head
      case _ => gcd((gcd(xs(0), xs(1)) +: xs.drop(2)): _*)
    }
  }
  def lcm(x: A, y: A): A = exactQuotient(multiply(x, y), gcd(x, y))
  def lcm(xs: A*): A = {
    xs.size match {
      case 0 => zero
      case 1 => xs.head
      case _ => lcm((lcm(xs(0), xs(1)) +: xs.drop(2)): _*)
    }
  }
}

trait EuclideanRig[@specialized(Int, Long, Float, Double) A] extends GCDRig[A] {
  def quotientRemainder(x: A, y: A): (A, A)
  def quotient(x: A, y: A): A = quotientRemainder(x, y)._1
  def remainder(x: A, y: A): A = quotientRemainder(x, y)._2

  override def exactQuotientOption(x: A, y: A) = {
    quotientRemainder(x, y) match {
      case (q, r) if zero_?(r) => Some(q)
      case _ => None
    }
  }

  @scala.annotation.tailrec
  final def euclideanAlgorithm(x: A, y: A): A = {
    if (zero_?(y)) {
      x
    } else {
      euclideanAlgorithm(y, remainder(x, y))
    }
  }

  override def gcd(x: A, y: A): A = euclideanAlgorithm(x, y)

  def digits(x: A, base: A = fromInt(10)): Seq[A] = {
    if (zero_?(x)) {
      Seq.empty
    } else {
      quotientRemainder(x, base) match {
        case (q, r) => digits(q, base) :+ r
      }
    }
  }
}

trait GCDRing[@specialized(Int, Long, Float, Double) A] extends GCDRig[A] with CommutativeRing[A]

trait EuclideanRing[@specialized(Int, Long, Float, Double) A] extends EuclideanRig[A] with GCDRing[A] with CommutativeRing[A] {
//  final def extendedEuclideanAlgorithm_(x: A, y: A): (A, A, A) = {
//    if (zero_?(y)) {
//      (one, zero, x)
//    } else {
//      val (a1, b1, g) = extendedEuclideanAlgorithm_(y, remainder(x, y))
//      (b1, subtract(a1, multiply(b1, quotient(x, y))), g)
//    }
//  }
  /**
   *
   * @param x
   * @param y
   * @return (a,b,g) such that a*x + b*y == g, and g is the gcd of x and y
   */
  final def extendedEuclideanAlgorithm(x: A, y: A): (A, A, A) = {
    var s1 = zero
    var s0 = one
    var t1 = s0
    var t0 = s1
    var r1 = y
    var r0 = x
    var z = s1
    while(!zero_?(r1)) {
      val q = quotient(r0, r1)
      z = r1
      r1 = subtract(r0, multiply(q, r1))
      r0 = z
      z = s1
      s1 = subtract(s0, multiply(q, s1))
      s0 = z
      z = t1
      t1 = subtract(t0, multiply(q, t1))
      t0 = z
    }
    (s0, t0, r0)
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

