package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.Field

trait MultivariablePolynomialAlgebraOverField[A, V] extends MultivariablePolynomialAlgebraOverGCDRing[A, V] {
  override implicit def ring: Field[A]

  case class GroebnerBasis(generators: Seq[MultivariablePolynomial[A, V]]) extends Ideal {
    val leadingMonomials = generators.groupBy(g => leadingMonomial(g).get)

    require(generators.forall({ p =>
      val lm = leadingMonomial(p).get
      p.coefficients.keys.forall(m => m.keys.forall(v => m(v) <= lm.getOrElse(v, 0)))
    }))

    def reduce(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
      var r = p
      var no: Option[MultivariablePolynomial[A, V]] = None
      while ({ no = reduceOneStep(r); no.nonEmpty }) {
        r = no.get
      }
      r
    }
    private def reduceOneStep(p: MultivariablePolynomial[A, V]): Option[MultivariablePolynomial[A, V]] = {
      p.coefficients.iterator.map({
        case (m, a) => {
          val lmo = leadingMonomials.keysIterator.find(lm => lm.forall(p => m.getOrElse(p._1, 0) >= p._2))
          (m, a, lmo)
        }
      }).find(_._3.nonEmpty).flatMap({
        case (m, a, Some(lm)) => {
          val g = leadingMonomials(lm).head
          Some(
            subtract(
              p,
              multiply(
                monomial(
                  implementation.keys.subtract(m, lm),
                  ring.quotient(a, g.coefficients(lm))),
                g)))
        }
        case _ => None
      })
    }
  }

}

object MultivariablePolynomialAlgebraOverField {
  implicit def over[A: Field, V: Ordering]: MultivariablePolynomialAlgebraOverField[A, V] = new MultivariablePolynomialAlgebraOverField[A, V] with MultivariablePolynomialAlgebraOverRig.LexicographicOrdering[A, V] {
    override val variableOrdering = implicitly[Ordering[V]]
    override val ring = implicitly[Field[A]]
  }
}

abstract class MultivariablePolynomialQuotientAlgebra[A: Field, V: Ordering] extends MultivariablePolynomialAlgebraOverField[A, V] {
  protected val polynomials = implicitly[MultivariablePolynomialAlgebraOverField[A, V]]

  override def ring = implicitly[Field[A]]
  override def variableOrdering = implicitly[Ordering[V]]

  def ideal: GroebnerBasis
  
  def normalForm(p: MultivariablePolynomial[A, V]) = ideal.reduce(p)
  
  override def multiply(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]) = {
    normalForm(polynomials.multiply(x, y))
  }
}

object MultivariablePolynomialAlgebras {
  def quotient[A: Field, V: Ordering](groebnerBasis: Seq[MultivariablePolynomial[A, V]]): MultivariablePolynomialQuotientAlgebra[A, V] = {
    val polynomials = implicitly[MultivariablePolynomialAlgebraOverField[A, V]]
    val ideal = polynomials.GroebnerBasis(groebnerBasis)
    quotient(polynomials)(ideal)
  }
  
  def quotient[A, V](a: MultivariablePolynomialAlgebraOverField[A, V])(i: a.GroebnerBasis): MultivariablePolynomialQuotientAlgebra[A, V] = {
    val ideal = a match {
      case a: MultivariablePolynomialQuotientAlgebra[A, V] => {
        // this require a Groebner basis algorithm to combine bases
        ???
      }
      case a => i
    }
    new MultivariablePolynomialQuotientAlgebra[A, V]()(a.ring, a.variableOrdering) {
      override val monomialOrdering = a.monomialOrdering
      override val ideal = GroebnerBasis(i.generators)
    }
  }
}

