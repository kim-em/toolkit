package net.tqft.toolkit.algebra

sealed trait Fraction[A] extends Serializable {
  def denominator: A
  def numerator: A

  override def toString = numerator.toString + " / " + denominator.toString
  override def equals(other: Any) = {
    other match {
      case other: Fraction[_] => numerator == other.numerator && denominator == other.denominator
      case _ => false
    }
  }
  override def hashCode = (numerator, denominator).hashCode
}

object Fraction {
  def apply[A](_numerator: A, _denominator: A)(implicit ring: EuclideanDomain[A]): Fraction[A] = {
    val gcd = ring.gcd(_numerator, _denominator)
    ring.quotient(_denominator, gcd) match {
      case q if q == ring.one =>
        new Fraction[A] {
          val numerator = ring.quotient(_numerator, gcd)
          val denominator = ring.one
          override def toString = numerator.toString
        }
      case _ =>
        new Fraction[A] {
          val numerator = ring.quotient(_numerator, gcd)
          val denominator = ring.quotient(_denominator, gcd)

          if (denominator.isInstanceOf[BigInt]) {
            if (denominator.asInstanceOf[BigInt].compare(BigInt(0)) < 0) require(false)
          }
        }
    }
  }

  def alreadyReduced[A](_numerator: A, _denominator: A): Fraction[A] = new Fraction[A] { val numerator = _numerator; val denominator = _denominator }

  implicit def toMathematicaExpression[A <% net.tqft.toolkit.mathematica.MathematicaExpression](f: Fraction[A]) = new net.tqft.toolkit.mathematica.ShortMathematicaExpression {
    def toMathematicaInputString = "(" + f.numerator.toMathematicaInputString + ")/(" + f.denominator.toMathematicaInputString + ")"
  }
}

trait NumberField[A] extends Field[Polynomial[A]] {
  def coefficientField: Field[A]
  val generator: Polynomial[A]
  lazy val degree = generator.maximumDegree.get

  protected lazy val polynomials = Polynomials.over(coefficientField) // has to be lazy so coefficientField is available

  private val powers = {
    import net.tqft.toolkit.functions.Memo._
    def f(n: Int) = polynomials.remainder(polynomials.monomial(n), generator)
    (f _).memo
  }
  def normalize(q: Polynomial[A]) = {
    q.maximumDegree match {
      case None => zero
      case Some(k) if k < degree => q
      case Some(k) if k < 2 * degree => {
        Polynomial((q.terms.flatMap {
          case (n, a) if n < degree => List((n, a))
          case (n, a) => powers(n).terms.map { case (m, b) => (m, coefficientField.multiply(a, b)) }
        }): _*)(coefficientField)
      }
      case _ => polynomials.remainder(q, generator)
    }
  }

  override def fromInt(x: Int) = polynomials.fromInt(x)
  override def inverse(q: Polynomial[A]) = {
    if (q == zero) throw new ArithmeticException("/ by zero")
    val (_, b, u) = (polynomials.extendedEuclideanAlgorithm(generator, q))
    require(u.maximumDegree == Some(0))
    scalarMultiply(coefficientField.inverse(u.constantTerm(coefficientField)), b)
  }
  override def negate(q: Polynomial[A]) = polynomials.negate(q)
  override lazy val zero = polynomials.zero
  override lazy val one = polynomials.one
  override def multiply(a: Polynomial[A], b: Polynomial[A]) = normalize(polynomials.multiply(a, b))
  override def add(a: Polynomial[A], b: Polynomial[A]) = polynomials.add(a, b)

  def scalarMultiply(a: A, p: Polynomial[A]): Polynomial[A] = polynomials.scalarMultiply(a, p)
}

trait ComplexConjugation[A] { self: Field[A] =>
  def bar(q: A): A
}

trait CyclotomicNumberField[A] extends NumberField[A] with ComplexConjugation[Polynomial[A]] {
  val order: Int
  override lazy val generator = Polynomial.cyclotomic(order)(coefficientField) // has to be lazy so coefficentField is available

  private def zeta = Polynomial.identity(coefficientField)
  private val zetaInversePowers = {
    import net.tqft.toolkit.functions.Memo._
    def f(n: Int) = inverse(power(zeta, n))
    (f _).memo
  }

  def bar(q: Polynomial[A]) = add(normalize(q).terms.map({ case (k, a) => scalarMultiply(a, zetaInversePowers(k)) }))
}

object NumberField {
  def apply[A: Field](p: Polynomial[A]): NumberField[A] = new NumberField[A] {
    override val generator = p
    override val coefficientField = implicitly[Field[A]]
  }

  def cyclotomic[A: Field](n: Int): CyclotomicNumberField[A] = new CyclotomicNumberField[A] {
    override val order = n
    override val coefficientField = implicitly[Field[A]]
  }
}

// needs lots of work!
object RealNumberField {
  def apply[I: IntegerModel, D: ApproximateReals](minimalPolynomial: Polynomial[I], approximation: D, epsilon: D): RealNumberField[I, D] = {
    new RealNumberField[I, D] {
      override val generator = minimalPolynomial.coefficientsAsFractions
      override val goodEnoughApproximation = approximation

      override var bestApproximation = approximation
      override var errorBound = epsilon

      override val integers = implicitly[IntegerModel[I]]
      override val approximateReals = implicitly[ApproximateReals[D]]
      override val coefficientField = Fields.fieldOfFractions(integers)
    }
  }
}

trait RealNumberField[I, D] extends NumberField[Fraction[I]] with OrderedField[Polynomial[Fraction[I]]] {
  val goodEnoughApproximation: D
  implicit val integers: IntegerModel[I]
  lazy val rationals = Fields.fieldOfFractions(integers)
  val approximateReals: ApproximateReals[D]

  protected var bestApproximation: D
  protected var errorBound: D
  private def errorBoundForPower(k: Int): D = k match {
    case 0 => approximateReals.zero
    case k => approximateReals.multiplyByInt(approximateReals.multiply(errorBound, approximateReals.power(bestApproximation, k - 1)), k)
  }

  def improveErrorBound = ???

  def evaluateAt(d: D)(p: Polynomial[Fraction[I]]): D = approximateReals.add(for ((i, c) <- p.terms) yield approximateReals.multiply(evaluateFraction(c), approximateReals.power(d, i)))
  def evaluateFraction(x: Fraction[I]) = approximateReals.quotient(approximateReals.fromInteger(x.numerator), approximateReals.fromInteger(x.denominator))

  def approximateWithin(epsilon: D)(p: Polynomial[Fraction[I]]): D = {
    for (i <- 0 until degree) {
      while (approximateReals.compare(approximateReals.multiply(errorBoundForPower(i), evaluateFraction(generator.coefficientOf(i).getOrElse(rationals.zero))), approximateReals.quotientByInt(epsilon, degree)) > 0) {
        improveErrorBound
      }
    }
    evaluateAt(bestApproximation)(p)
  }

  override def compare(x: Polynomial[Fraction[I]], y: Polynomial[Fraction[I]]) = {
    if (x == y) {
      0
    } else {
      var epsilon = approximateReals.fromDouble(0.0001)
      def gap = approximateReals.subtract(approximateWithin(epsilon)(x), approximateWithin(epsilon)(y))
      while (approximateReals.compare(approximateReals.abs(gap), approximateReals.multiplyByInt(epsilon, 4)) < 0) {
        epsilon = approximateReals.quotientByInt(epsilon, 10)
      }
      approximateReals.compare(gap, approximateReals.zero).ensuring(_ != 0)
    }
  }
}

object Mod {
  def apply(p: Int): Field[Int] with Elements[Int] = {
    require(p < scala.math.sqrt(Integer.MAX_VALUE))
    require(BigInt(p).isProbablePrime(20))
    new Field[Int] with Elements[Int] {
      import net.tqft.toolkit.arithmetic.Mod._

      override def elements = (0 until p).toSet
      override def inverse(x: Int) = {
        if (x == 0) throw new ArithmeticException("/ by zero")
        Gadgets.Integers.extendedEuclideanAlgorithm(x, p)._1
      }
      override def negate(x: Int) = (p - x) mod p
      override val zero = 0
      override val one = 1
      override def multiply(x: Int, y: Int) = (x * y) mod p
      override def add(x: Int, y: Int) = (x + y) mod p
      override def fromInt(x: Int) = x mod p

    }
  }
}

object Fields extends HomomorphismCategory[Field] {
  private def reduceFraction[A](ring: EuclideanDomain[A], numerator: A, denominator: A): (A, A) = {
    val gcd = ring.gcd(numerator, denominator)
    (ring.quotient(numerator, gcd), ring.quotient(denominator, gcd))
  }

  val embeddingInFieldOfFractions = new NaturalTransformation[EuclideanDomain, EuclideanDomain, Functors.Identity, Fraction] {
    def source = Functors.Identity(EuclideanDomains)

    // TODO why aren't types inferred here?
    def target = fieldOfFractions.andThen[EuclideanDomain, EuclideanDomains.Homomorphism, Functors.Identity](Functors.Forget(Fields, EuclideanDomains))

    def apply[A](o: EuclideanDomain[A]): Homomorphism[EuclideanDomain, A, Fraction[A]] = new Homomorphism[EuclideanDomain, A, Fraction[A]] {
      def source = o
      def target = fieldOfFractions(o)
      def apply(a: A) = Fraction(a, o.one)(o)
    }
  }

  class FieldOfFractions[A](ring: EuclideanDomain[A]) extends Field[Fraction[A]] {
    implicit val _ring = ring
    override val one = Fraction.alreadyReduced(ring.one, ring.one)
    override val zero = Fraction.alreadyReduced(ring.zero, ring.one)
    override def multiply(x: Fraction[A], y: Fraction[A]) = Fraction(ring.multiply(x.numerator, y.numerator), ring.multiply(x.denominator, y.denominator))
    override def add(x: Fraction[A], y: Fraction[A]) = {
      val denominatorGCD = ring.gcd(x.denominator, y.denominator)
      Fraction(ring.add(ring.multiply(x.numerator, ring.quotient(y.denominator, denominatorGCD)), ring.multiply(ring.quotient(x.denominator, denominatorGCD), y.numerator)), ring.multiply(ring.quotient(x.denominator, denominatorGCD), y.denominator))
    }
    override def fromInt(x: Int) = Fraction.alreadyReduced(ring.fromInt(x), ring.one)
    override def negate(x: Fraction[A]) = Fraction.alreadyReduced(ring.negate(x.numerator), x.denominator)
    override def inverse(x: Fraction[A]) = Fraction.alreadyReduced(x.denominator, x.numerator)
  }

  class OrderedFieldOfFractions[A](ring: OrderedEuclideanDomain[A]) extends FieldOfFractions[A](ring) with OrderedField[Fraction[A]] {
    def compare(x: Fraction[A], y: Fraction[A]) = ring.compare(ring.multiply(x.numerator, y.denominator), ring.multiply(y.numerator, x.denominator))
  }

  object Rationals extends OrderedFieldOfFractions(Gadgets.Integers)

  val fieldOfFractions = new Functor[EuclideanDomain, Field, Fraction] { self =>
    def source = EuclideanDomains
    def target = Fields
    def apply[A](ring: EuclideanDomain[A]): Field[Fraction[A]] = new FieldOfFractions(ring)
    def apply[A](ring: OrderedEuclideanDomain[A]): OrderedField[Fraction[A]] = new OrderedFieldOfFractions(ring)
    def apply[A, B](hom: Homomorphism[EuclideanDomain, A, B]): FieldHomomorphism[Fraction[A], Fraction[B]] = new FieldHomomorphism[Fraction[A], Fraction[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(m: Fraction[A]) = Fraction(hom(m.numerator), hom(m.denominator))(hom.target)
    }
  }
}