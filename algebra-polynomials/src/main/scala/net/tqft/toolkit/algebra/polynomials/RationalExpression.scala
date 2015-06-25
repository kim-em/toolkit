package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.Integers
import scala.language.implicitConversions
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.GCDRing

sealed trait RationalExpression[A, V] {
  def variables: Set[V]
  def simplify(implicit field: Field[A]): RationalExpression[A, V] = this
}

object RationalExpression {
  case class constant[A, V](a: A) extends RationalExpression[A, V] {
    override def variables = Set.empty
  }
  case class variable[A, V](v: V) extends RationalExpression[A, V] {
    override def variables = Set(v)
  }
  case class power[A, V](r: RationalExpression[A, V], k: Int) extends RationalExpression[A, V] {
    override def variables = r.variables
    override def simplify(implicit field: Field[A]) = {
      r match {
        case constant(a) => constant(field.power(a, k))
        case power(s, l) => power(s, k * l)
        case _ => {
          if (k == 1) {
            r
          } else if (k == 0) {
            constant(field.one)
          } else {
            this
          }
        }
      }
    }
  }
  case class sum[A, V](terms: RationalExpression[A, V]*) extends RationalExpression[A, V] {
    override def variables = terms.flatMap(_.variables).toSet
    override def simplify(implicit field: Field[A]) = {
      val flatten = terms.flatMap({
        case sum(subterms @ _*) => subterms
        case e => Seq(e)
      })
      val (constants, nonconstants) = flatten.partition(_.isInstanceOf[constant[A, V]])
      val constantSum = field.sum(constants.map(_.asInstanceOf[constant[A, V]].a))
      import net.tqft.toolkit.collections.Tally._
      val newNonConstantTerms = nonconstants.tally.map({
        case (x, 1) => x
        case (x, k) => product(constant[A, V](field.fromInt(k)), x).simplify
      })
      val newConstantTerms = if (field.zero_?(constantSum)) Seq.empty else Seq(constant[A, V](constantSum))
      val newTerms = newConstantTerms ++ newNonConstantTerms
      if (newTerms.isEmpty) {
        constant[A, V](field.zero)
      } else {
        sum(newTerms: _*)
      }
    }
  }
  case class product[A, V](factors: RationalExpression[A, V]*) extends RationalExpression[A, V] {
    override def variables = factors.flatMap(_.variables).toSet
    override def simplify(implicit field: Field[A]) = {
      val flatten = factors.flatMap({
        case sum(subfactors @ _*) => subfactors
        case e => Seq(e)
      })
      val (constants, nonconstants) = flatten.partition(_.isInstanceOf[constant[A, V]])
      val constantProduct = field.product(constants.map(_.asInstanceOf[constant[A, V]].a))
      import net.tqft.toolkit.collections.Tally._
      val newNonConstantFactors = nonconstants.tally.map({
        case (x, 1) => x
        case (x, k) => power(x, k).simplify
      })
      val newConstantFactors = if (field.one_?(constantProduct)) Seq.empty else Seq(constant[A, V](constantProduct))
      val newFactors = newConstantFactors ++ newNonConstantFactors
      if (newFactors.isEmpty) {
        constant[A, V](field.one)
      } else {
        product(newFactors: _*)
      }
    }

  }

  implicit def liftConstant[A, V](a: A): RationalExpression[A, V] = constant(a)
  implicit def liftVariable[A, V](v: V): RationalExpression[A, V] = variable(v)
  implicit def liftFraction[A:GCDRing, V](a: A): RationalExpression[Fraction[A], V] = constant(a)
  implicit def liftInt[A:GCDRing, V](i: Int): RationalExpression[Fraction[A], V] = constant(implicitly[GCDRing[A]].fromInt(i))
  
  implicit def fieldOfRationalExpressions[A: Field, V]: Field[RationalExpression[A, V]] = new Field[RationalExpression[A, V]] {
    private def field = implicitly[Field[A]]
    override val zero = RationalExpression.constant[A, V](field.zero)
    override val one = RationalExpression.constant[A, V](field.one)
    override val negativeOne = RationalExpression.constant[A, V](field.negativeOne)
    override def negate(x: RationalExpression[A, V]) = RationalExpression.product(RationalExpression.constant[A, V](field.negativeOne), x).simplify
    override def inverse(x: RationalExpression[A, V]) = power[Int](x, -1).simplify
    override def add(x: RationalExpression[A, V], y: RationalExpression[A, V]) = RationalExpression.sum(x, y).simplify
    override def multiply(x: RationalExpression[A, V], y: RationalExpression[A, V]) = RationalExpression.product(x, y).simplify
    override def power[I: IntegerModel](x: RationalExpression[A, V], k: I) = RationalExpression.power(x, Integers.from(k)).simplify
  }
}