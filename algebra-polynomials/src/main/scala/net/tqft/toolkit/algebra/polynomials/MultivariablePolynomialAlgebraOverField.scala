package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.GCDRing
import net.tqft.toolkit.algebra.Fraction

trait MultivariablePolynomialAlgebraOverField[A, V] extends MultivariablePolynomialAlgebraOverGCDRing[A, V] {
  override implicit def ring: Field[A]

  case class GroebnerBasis(generators: Seq[MultivariablePolynomial[A, V]]) extends Ideal {
    lazy val leadingMonomials = generators.groupBy(g => leadingMonomial(g).get).mapValues(v => v.ensuring(_.size == 1).head)

    for ((lm, g) <- leadingMonomials) {
      //      println((lm, g))
      require(leadingMonomial(g).get == lm)
    }

    def reduce(p: MultivariablePolynomial[A, V]): MultivariablePolynomial[A, V] = {
      var r = p
      var no: Option[MultivariablePolynomial[A, V]] = None
      while ({ no = reduceOneStep(r); no.nonEmpty }) {
        r = no.get
      }
      r
    }
    private def reduceOneStep(p: MultivariablePolynomial[A, V]): Option[MultivariablePolynomial[A, V]] = {
      //      println(s"reduceOneStep($p)")

      p.coefficients.toSeq.sortBy(_._1)(monomialOrdering).reverse.iterator.map({
        case (m, a) => {
          val lmo = leadingMonomials.keysIterator.find(lm => lm.forall(p => m.getOrElse(p._1, 0) >= p._2))
          (m, a, lmo)
        }
      }).find(_._3.nonEmpty).flatMap({
        case (m, a, Some(lm)) => {
          val g = leadingMonomials(lm)
          require(leadingMonomial(g).get == lm)
          //          println(s"found: m = $m, a = $a, lm = $lm")
          //          println(s"       g = $g, g.coefficients(lm) = ${g.coefficients(lm)}")
          //          println(g.coefficients.map(_._1).toSeq.sorted(monomialOrdering).reverse)
          //          println(g.coefficients.map(_._1).toSeq.sorted(monomialOrdering).reverse.take(2).sorted(monomialOrdering).reverse)
          //          println(s"       leadingMonomial(g).get = ${leadingMonomial(g).get}")
          val result = subtract(
            p,
            multiply(
              monomial(
                implementation.keys.subtract(m, lm),
                ring.quotient(a, g.coefficients(lm))),
              g))
          //              println(s"result: $result")
          require(monomialOrdering.compare(leadingMonomial(result).getOrElse(Map.empty), leadingMonomial(p).get) <= 0)
          implicit def mo: Ordering[Map[V, Int]] = monomialOrdering
          import Ordering.Implicits._
          val oo = Ordering.Implicits.seqDerivedOrdering[Seq, Map[V, Int]](monomialOrdering)
          val pcoeffs = p.coefficients.toSeq.map(_._1).sorted(monomialOrdering).reverse
          val rcoeffs = result.coefficients.toSeq.map(_._1).sorted(monomialOrdering).reverse
          //          println(pcoeffs)
          //          println(rcoeffs)
          require(oo.compare(rcoeffs, pcoeffs) < 0)
          Some(result)
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

  override def gcd(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]) = normalForm(polynomials.gcd(x, y))
  override def gcdWithQuotients(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]) = {
    val (g, xq, yq) = polynomials.gcdWithQuotients(x, y)
    (normalForm(g), normalForm(xq), normalForm(yq))
  }
  override def lcm(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]) = normalForm(polynomials.lcm(x, y))

  override def ring = implicitly[Field[A]]
  override def variableOrdering = implicitly[Ordering[V]]

  def ideal: polynomials.GroebnerBasis

  def normalForm(p: MultivariablePolynomial[A, V]) = ideal.reduce(p)

  override def multiply(x: MultivariablePolynomial[A, V], y: MultivariablePolynomial[A, V]) = {
    normalForm(polynomials.multiply(x, y))
  }

  override def toString = s"MultivariablePolynomialAlgebras.quotient(${ideal.generators})"
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
      override val ideal = polynomials.GroebnerBasis(i.generators)
    }
  }
}

