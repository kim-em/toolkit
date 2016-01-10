package net.tqft.toolkit.algebra

trait AlgebraicExpression[A, B]

object AlgebraicExpression {
  trait Sum[A, B] extends AlgebraicExpression[A, B] {
    def terms: Seq[AlgebraicExpression[A, B]]
  }
  trait Product[A, B] extends AlgebraicExpression[A, B] {
    def scalar: Option[A]
    def factors: Seq[AlgebraicExpression[A, B]]
  }
  trait Exp[A, B] extends AlgebraicExpression[A, B] {
    def base: AlgebraicExpression[A, B]
    def power: Int
  }

  case class Atom[A, B](expression: B) extends AlgebraicExpression[A, B]
  case class Constant[A, B](value: A) extends AlgebraicExpression[A, B]

  case class Plus[A, B](terms: Seq[AlgebraicExpression[A, B]]) extends Sum[A, B]
  case class Times[A, B](scalar: Option[A], factors: Seq[AlgebraicExpression[A, B]]) extends Product[A, B]
  case class Power[A, B](base: AlgebraicExpression[A, B], power: Int) extends Exp[A, B]

  case class Polynomial[A, B](terms: Seq[Monomial[A, B]]) extends Sum[A, B]
  case class Monomial[A, B](scalar: Option[A], factors: Seq[UnivariateMonomial[A, B]]) extends Product[A, B]
  case class UnivariateMonomial[A, B](base: Atom[A, B], power: Int) extends Exp[A, B]

  class RingOfAlgebraicExpression[A: Ring, B] extends Ring[AlgebraicExpression[A, B]] {
    def ring = implicitly[Ring[A]]

    override def zero = Constant(ring.zero)
    override def one = Constant(ring.one)

    override def negate(x: AlgebraicExpression[A, B]): AlgebraicExpression[A, B] = {
      def negateMonomial(x: Monomial[A, B]): Monomial[A, B] = {
        x match {
          case Monomial(None, factors) => Monomial(Some(ring.fromInt(-1)), factors)
          case Monomial(Some(a), factors) => {
            val na = ring.negate(x.scalar.get)
            if (na == ring.one) {
              Monomial(None, factors)
            } else {
              Monomial(Some(na), factors)
            }
          }
        }
      }
      def negateTimes(x: Times[A, B]): Times[A, B] = {
        x match {
          case Times(None, factors) => Times(Some(ring.fromInt(-1)), factors)
          case Times(Some(a), factors) => {
            val na = ring.negate(x.scalar.get)
            if (na == ring.one) {
              Times(None, factors)
            } else {
              Times(Some(na), factors)
            }
          }
        }
      }
      x match {
        case Constant(a) => Constant(ring.negate(a))
        case Atom(b) => negate(UnivariateMonomial(Atom[A, B](b), 1))
        case x: UnivariateMonomial[A, B] => negate(Monomial(None, Seq(x)))
        case x: Monomial[A, B] => negateMonomial(x)
        case Polynomial(terms) => Polynomial(terms.map(negateMonomial))
        case x: Power[A, B] => Times(Some(ring.fromInt(-1)), Seq(x))
        case x: Times[A, B] => negateTimes(x)
        case Plus(terms) => Plus(terms.map(negate))
      }
    }
    override def add(x: AlgebraicExpression[A, B], y: AlgebraicExpression[A, B]): AlgebraicExpression[A, B] = {
      ???
    }
    override def multiply(x: AlgebraicExpression[A, B], y: AlgebraicExpression[A, B]): AlgebraicExpression[A, B] = {
      ???
    }
  }
}