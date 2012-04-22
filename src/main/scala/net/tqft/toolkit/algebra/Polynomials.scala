package net.tqft.toolkit.algebra
import net.tqft.toolkit.mathematica.MathematicaExpression
import net.tqft.toolkit.mathematica.ShortMathematicaExpression

trait Polynomial[A] extends LinearCombo[A, Int] {
import net.tqft.toolkit.arithmetic.MinMax._
import net.tqft.toolkit.algebra.AlgebraicNotation
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.Functor
import net.tqft.toolkit.algebra.Homomorphism
import net.tqft.toolkit.algebra.HomomorphismCategory
import net.tqft.toolkit.algebra.LinearCombo
import net.tqft.toolkit.algebra.Polynomial
import net.tqft.toolkit.algebra.PolynomialAlgebra
import net.tqft.toolkit.algebra.PolynomialAlgebraOverField
import net.tqft.toolkit.algebra.Polynomials
import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.RingHomomorphism
import net.tqft.toolkit.algebra.Rings
  
  override def toString = if(terms.isEmpty) {
    "0"
  } else {
    (terms map { case (g, p) => p.toString + " * x^(" + g.toString + ")"}).mkString(" + ")
  }
  
  def minimumDegree = (terms map { _._1 }) minOption
  def maximumDegree = (terms map { _._1 }) maxOption
  def leadingCoefficient = maximumDegree map { get(_).get }
  def constantTerm(implicit ring: Ring[A]) = get(0).getOrElse(ring.zero)
}

object Polynomial {
  def apply[A](terms: (Int, A)*)(implicit ring: Ring[A]) = Polynomials.over(ring).wrap(terms.toList)
  def apply[A](terms: Map[Int, A])(implicit ring: Ring[A]) = Polynomials.over(ring).wrap(terms)

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
      val ring = _ring      
    }
    
    def apply[A](_ring: Ring[A]): PolynomialAlgebra[A] = new PolynomialAlgebra[A] {
      val ring = _ring      
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

}
