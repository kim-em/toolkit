package net.tqft.toolkit.algebra
import net.tqft.toolkit.mathematica.MathematicaExpression
import net.tqft.toolkit.mathematica.ShortMathematicaExpression

trait Polynomial[A] extends LinearCombo[A, Int] { polynomial =>
  import net.tqft.toolkit.arithmetic.MinMax._
 
  override def toString = if (terms.isEmpty) {
    "0"
  } else {
    (terms map { case (g, p) => p.toString + (if(g == 0) "" else " * x^(" + g.toString + ")") }).mkString(" + ")
  }

  def minimumDegree = (terms map { _._1 }) minOption
  def maximumDegree = (terms map { _._1 }) maxOption
  def leadingCoefficient = maximumDegree map { get(_).get }
  def constantTerm(implicit ring: Ring[A]) = get(0).getOrElse(ring.zero)
  
  def roots(implicit ring: Ring[A] with Elements[A]) = {
    for(x <- ring.elements; if Polynomials.evaluateAt(x).apply(polynomial) == ring.zero) yield x
  }
}

object Polynomial {
  def apply[A](terms: (Int, A)*)(implicit ring: Ring[A]) = Polynomials.over(ring).wrap(terms.toList)
  def apply[A](terms: Map[Int, A])(implicit ring: Ring[A]) = Polynomials.over(ring).wrap(terms)

  def identity[A](implicit ring: Ring[A]) = apply((1, ring.one))

  def cyclotomic[A:Field](n: Int): Polynomial[A] = {
    val field = implicitly[Field[A]]
    val polynomials = Polynomials.over(field)
    val divisors = for(d <- 1 until n; if n % d == 0) yield cyclotomic(d)
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

  def evaluateAt[A](x: A)(implicit ring: Ring[A]) = new Homomorphism[Ring, Polynomial[A], A] {
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

  def evaluateAt[A](x: A)(implicit field: Field[A]) = new Homomorphism[Field, RationalFunction[A], A] {
    def source: Field[RationalFunction[A]] = over[A](field)
    def target: Field[A] = field
    def apply(p: RationalFunction[A]) = field.quotient(Polynomials.evaluateAt(x).apply(p.numerator), Polynomials.evaluateAt(x).apply(p.denominator))
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