package net.tqft.toolkit.algebra
import net.tqft.toolkit.mathematica.MathematicaExpression
import net.tqft.toolkit.mathematica.ShortMathematicaExpression

trait Polynomial[A] extends LinearCombo[A, Int] { polynomial =>
  import net.tqft.toolkit.arithmetic.MinMax._

  override def toString = if (terms.isEmpty) {
    "0"
  } else {
    (terms map { case (g, p) => p.toString + (if (g == 0) "" else " * x^(" + g.toString + ")") }).mkString(" + ")
  }

  def minimumDegree = (terms map { _._1 }).minOption
  def maximumDegree = (terms map { _._1 }).maxOption
  def degree = maximumDegree.get
  def leadingCoefficient = maximumDegree map { coefficientOf(_).get }
  def constantTerm(implicit ring: Ring[A]) = coefficientOf(0).getOrElse(ring.zero)

  
  
  def roots(implicit ring: Ring[A] with Elements[A]) = {
    for (x <- ring.elements; if Polynomials.evaluationAt(x).apply(polynomial) == ring.zero) yield x
  }

  def coefficientsAsFractions(implicit domain: EuclideanDomain[A]): Polynomial[Fraction[A]] = {
    implicit val fractions = Fields.fieldOfFractions(domain)
    Polynomial(terms.map({ case (i, a) => (i, Fraction.alreadyReduced(a, domain.one)) }): _*)
  }
}

object Polynomial {
  def apply[A: Ring](terms: (Int, A)*) = Polynomials.over(implicitly[Ring[A]]).wrap(terms.toList)
  def apply[A: Ring](terms: Map[Int, A]) = Polynomials.over(implicitly[Ring[A]]).wrap(terms)
  def constant[A: Ring](x: A) = apply((0, x))
  def identity[A: Ring] = apply((1, implicitly[Ring[A]].one))

  def cyclotomic[A: Field](n: Int): Polynomial[A] = {
    val field = implicitly[Field[A]]
    val polynomials = Polynomials.over(field)
    val divisors = for (d <- 1 until n; if n % d == 0) yield cyclotomic(d)
    polynomials.quotient(apply((0, field.negate(field.one)), (n, field.one)), polynomials.multiply(divisors))
  }

  // TODO move this somewhere else?
  implicit def asMathematicaExpression[A <% MathematicaExpression](p: Polynomial[A]) = new ShortMathematicaExpression {
    val symbol = "x" // TODO work out how to allow varying this?

    def toMathematicaInputString = {
      if (p.terms.isEmpty) {
        // TODO we need a ring in scope:
        "0"
      } else {
        (for ((b, a) <- p.terms) yield a.toMathematicaInputString + " " + symbol + "^(" + b + ")").mkString(" + ")
      }
    }
  }
}

object Polynomials extends HomomorphismCategory[PolynomialAlgebra] {

  val over = new Functor[Ring, Ring, Polynomial] { self =>
    def source = Rings
    def target = Rings

    def apply[A](_ring: Field[A]): PolynomialAlgebraOverField[A] = new PolynomialAlgebraOverField[A] {
      override val ring = _ring
    }

    def apply[A](_ring: Ring[A]): PolynomialAlgebra[A] = new PolynomialAlgebra[A] {
      override val ring = _ring
    }

    def apply[A, B](hom: Homomorphism[Ring, A, B]): Homomorphism[Ring, Polynomial[A], Polynomial[B]] = new RingHomomorphism[Polynomial[A], Polynomial[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(p: Polynomial[A]): Polynomial[B] = new Polynomial[B] {
        def terms = p.terms map { case (k: Int, a) => (k, hom(a)) }
      }
    }
  }

  def evaluationAt[A](x: A)(implicit ring: Ring[A]) = new Homomorphism[Ring, Polynomial[A], A] {
    def source: Ring[Polynomial[A]] = over[A](ring)
    def target: Ring[A] = ring
    def apply(p: Polynomial[A]) = AlgebraicNotation.sum(p.terms map { case (e, a) => ring.multiply(a, ring.power(x, e)) })
  }

  val embeddingAsConstants = new NaturalTransformation[Ring, Ring, Functors.Identity, Polynomial] {
    def source = Functors.Identity(Rings)

    def target = over

    def apply[A](o: Ring[A]): Homomorphism[Ring, A, Polynomial[A]] = new Homomorphism[Ring, A, Polynomial[A]] {
      def source = o
      def target = over(o)
      def apply(a: A) = Polynomial((0, a))(o)
    }
  }

}

object RationalFunction {
  def identity[A](implicit ring: Field[A]) = Fraction(Polynomial.identity, Polynomials.over(ring).one)(Polynomials.over(ring))
}

object RationalFunctions {
  type RationalFunction[A] = Fraction[Polynomial[A]]

  val over = new Functor[Field, Field, RationalFunction] { self =>
    def source = Fields
    def target = Fields

    def apply[A](_field: Field[A]) = Fields.fieldOfFractions(Polynomials.over(_field))

    def apply[A, B](hom: Homomorphism[Field, A, B]): Homomorphism[Field, RationalFunction[A], RationalFunction[B]] = new FieldHomomorphism[RationalFunction[A], RationalFunction[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(f: RationalFunction[A]): RationalFunction[B] = {
        implicit val bPolynomials = Polynomials.over(hom.target)
        val polynomialHom = Polynomials.over(hom)
        Fraction(polynomialHom(f.numerator), polynomialHom(f.denominator))
      }
    }
  }

  def evaluationAt[A](x: A)(implicit field: Field[A]) = new Homomorphism[Field, RationalFunction[A], A] {
    def source: Field[RationalFunction[A]] = over[A](field)
    def target: Field[A] = field
    def apply(p: RationalFunction[A]) = field.quotient(Polynomials.evaluationAt(x).apply(p.numerator), Polynomials.evaluationAt(x).apply(p.denominator))
  }

  val embeddingAsConstants = new NaturalTransformation[Field, Field, Functors.Identity, RationalFunction] {
    def source = Functors.Identity(Fields)

    def target = over

    def apply[A](o: Field[A]): Homomorphism[Field, A, RationalFunction[A]] = new Homomorphism[Field, A, RationalFunction[A]] {
      def source = o
      def target = over(o)
      def apply(a: A) = {
        implicit val polynomials = Polynomials.over(o)
        Fraction(Polynomial((0, a))(o), Polynomial((0, o.one))(o))
      }
    }
  }

}

trait PolynomialAlgebra[A] extends FreeModuleOnMonoid[A, Int, Polynomial[A]] with AssociativeAlgebra[A, Polynomial[A]] {

  def monomial(k: Int): Polynomial[A] = monomial(k, ring.one)
  def monomial(k: Int, a: A): Polynomial[A] = Polynomial((k, a))

  def constant(a: A) = monomial(0, a)
  override def fromInt(x: Int): Polynomial[A] = constant(ring.fromInt(x))

  override val monoid = Gadgets.Integers

  override def wrap(terms: List[(Int, A)]): Polynomial[A] = new PolynomialImpl(terms)
  private class PolynomialImpl(_terms: List[(Int, A)]) extends Polynomial[A] {
    val terms = reduce(_terms)
  }

  def composeAsFunctions(p: Polynomial[A], q: Polynomial[A]): Polynomial[A] = {
    add(p.terms map { case (e, a) => scalarMultiply(a, power(q, e)) })
  }

  def formalDerivative(p: Polynomial[A]): Polynomial[A] = {
    Polynomial((p.terms map { case (0, _) => (0, ring.zero); case (k, a) => (k - 1, ring.multiplyByInt(a, k)) }): _*)
  }
}

trait PolynomialAlgebraOverField[A] extends PolynomialAlgebra[A] with EuclideanDomain[Polynomial[A]] {
  override implicit def ring: Field[A]

  def quotientRemainder(x: Polynomial[A], y: Polynomial[A]): (Polynomial[A], Polynomial[A]) = {
    (x.maximumDegree, y.maximumDegree) match {
      case (_, None) => throw new ArithmeticException
      case (None, Some(dy)) => (zero, zero)
      case (Some(dx), Some(dy)) => {
        if (dy > dx) {
          (zero, x)
        } else {
          val ax = x.leadingCoefficient.get
          val ay = y.leadingCoefficient.get

          require(ax != ring.zero)
          require(ay != ring.zero)

          val q = ring.quotient(ax, ay)

          val quotientLeadingTerm = monomial(dx - dy, q)
          val difference = add(x, negate(multiply(quotientLeadingTerm, y)))
//          require(difference.coefficientOf(dx) == None)
          val (restOfQuotient, remainder) = quotientRemainder(difference, y)

          (add(quotientLeadingTerm, restOfQuotient), remainder)
        }
      }
    }
  }

  def removeMultipleRoots(p: Polynomial[A]): Polynomial[A] = {
    quotient(p, gcd(p, formalDerivative(p)))
  }
}

case class Interval[D](lower: D, upper: D) {
  def width(implicit field: OrderedField[D]) = field.subtract(upper, lower)
  def midpoint(implicit field: OrderedField[D]) = field.quotientByInt(field.add(upper, lower), 2)
}

trait PolynomialSolver[A] {
  type Embedding[D] = A => D
  def approximateSimpleRootWithin[D: OrderedField: Embedding](epsilon: D)(p: Polynomial[A])(bounds: Interval[D]): Interval[D]
}

// the bisection method is quite slow, but at least this gets us off the ground
trait BisectionMethod[A] extends PolynomialSolver[A] {
  override def approximateSimpleRootWithin[D: OrderedField: Embedding](epsilon: D)(p: Polynomial[A])(bounds: Interval[D]): Interval[D] = {

    val field = implicitly[OrderedField[D]]    
    def evaluateAt(x: D): D = field.add(p.terms map { case (e, a) => field.multiply(a, field.power(x, e)) })
    
    // if the function isn't increasing across the interval, fake it
    val increasing = field.signum(evaluateAt(bounds.upper)) > 0
    def evaluateIncreasingAt(x: D): D = if(increasing) {
      evaluateAt(x)
    } else {
      field.negate(evaluateAt(x))
    }
    
    @scala.annotation.tailrec
    def impl(bounds: Interval[D]): Interval[D] = {
      if (field.compare(bounds.width, field.multiplyByInt(epsilon, 2)) < 0) {
        bounds
      } else {
        val midpoint = bounds.midpoint
        if (field.signum(evaluateIncreasingAt(midpoint)) < 0) {
          impl(Interval(midpoint, bounds.upper))
        } else {
          impl(Interval(bounds.lower, midpoint))
        }
      }
    }

    impl(bounds)
  }
}