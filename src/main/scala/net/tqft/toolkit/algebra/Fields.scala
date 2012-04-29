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
}

object Fraction {
  def apply[A](_numerator: A, _denominator: A)(implicit ring: EuclideanDomain[A]): Fraction[A] = {
    val gcd = ring.gcd(_numerator, _denominator)
    new Fraction[A] {
      val numerator = ring.quotient(_numerator, gcd)
      val denominator = ring.quotient(_denominator, gcd)
      
      if(denominator.isInstanceOf[BigInt]) {
        if(denominator.asInstanceOf[BigInt].compare(BigInt(0)) < 0) require(false)
      }
    }
  }
  
  def alreadyReduced[A](_numerator: A, _denominator: A): Fraction[A] = new Fraction[A] { val numerator = _numerator; val denominator = _denominator }
  
  implicit def toMathematicaExpression[A <% net.tqft.toolkit.mathematica.MathematicaExpression](f: Fraction[A]) = new net.tqft.toolkit.mathematica.ShortMathematicaExpression {
    def toMathematicaInputString = "(" + f.numerator.toMathematicaInputString + ")/(" + f.denominator.toMathematicaInputString + ")"
  }
}

object NumberField {
  def apply[A](p: Polynomial[A])(implicit field: Field[A]) = new Field[Polynomial[A]] {
    val polynomials = Polynomials.over(field)
    
    def normalize(q: Polynomial[A]) = polynomials.remainder(q, p)
    
    def inverse(q: Polynomial[A]) = (extendedEuclideanAlgorithm(p, q) ensuring { _._3 == one })._1
    def negate(q: Polynomial[A]) = polynomials.negate(q)
    def zero = polynomials.zero
    def one = normalize(polynomials.one)
    def multiply(a: Polynomial[A], b: Polynomial[A]) = normalize(polynomials.multiply(a, b))
    def add(a: Polynomial[A], b: Polynomial[A]) = polynomials.multiply(a, b)
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
      def one = Fraction(ring.one, ring.one)
      def zero = Fraction(ring.zero, ring.one)
      def multiply(x: Fraction[A], y: Fraction[A]) = Fraction(ring.multiply(x.numerator, y.numerator), ring.multiply(x.denominator, y.denominator))
      def add(x: Fraction[A], y: Fraction[A]) = {
        val denominatorGCD = ring.gcd(x.denominator, y.denominator)
        Fraction(ring.add(ring.multiply(x.numerator, ring.quotient(y.denominator, denominatorGCD)), ring.multiply(ring.quotient(x.denominator, denominatorGCD), y.numerator)), ring.multiply(ring.quotient(x.denominator, denominatorGCD), y.denominator))
      }
      def negate(x: Fraction[A]) = Fraction(ring.negate(x.numerator), x.denominator)
      def inverse(x: Fraction[A]) = Fraction(x.denominator, x.numerator)
    }
  
  val fieldOfFractions = new Functor[EuclideanDomain, Field, Fraction] { self =>
    def source = EuclideanDomains
    def target = Fields
    def apply[A](ring: EuclideanDomain[A]): Field[Fraction[A]] =  new FieldOfFractions(ring)
    def apply[A](ring: OrderedEuclideanDomain[A]): OrderedField[Fraction[A]] = new FieldOfFractions(ring) with OrderedField[Fraction[A]] {
      def compare(x: Fraction[A], y: Fraction[A]) = ring.compare(ring.multiply(x.numerator, y.denominator), ring.multiply(y.numerator, x.denominator))
    }
    def apply[A, B](hom: Homomorphism[EuclideanDomain, A, B]): FieldHomomorphism[Fraction[A], Fraction[B]] = new FieldHomomorphism[Fraction[A], Fraction[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(m: Fraction[A]) = Fraction(hom(m.numerator), hom(m.denominator))(hom.target)
    }
  }
}