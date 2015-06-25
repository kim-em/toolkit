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
  implicit def ordering[A: Ordering, V: Ordering]: Ordering[RationalExpression[A, V]] = new Ordering[RationalExpression[A, V]] {
    override def compare(x: RationalExpression[A, V], y: RationalExpression[A, V]) = {
      import Ordering.Implicits._

      x match {
        case constant(xa) => {
          y match {
            case constant(ya) => implicitly[Ordering[A]].compare(xa, ya)
            case _ => -1
          }
        }
        case variable(xv) => {
          y match {
            case constant(_) => 1
            case variable(yv) => implicitly[Ordering[V]].compare(xv, yv)
            case _ => -1
          }
        }
        case power(xr, xk) => {
          y match {
            case constant(_) | variable(_) => 1
            case power(yr, yk) => {
              compare(xr, yr) match {
                case 0 => xk - yk
                case d => d
              }
            }
            case _ => -1
          }
        }
        case sum(xterms @ _*) => {
          y match {
            case constant(_) | variable(_) | power(_, _) => 1
            case sum(yterms @ _*) => implicitly[Ordering[Seq[RationalExpression[A, V]]]].compare(xterms, yterms)
            case _ => -1
          }
        }
        case product(xfactors @ _*) => {
          y match {
            case product(yfactors @ _*) => implicitly[Ordering[Seq[RationalExpression[A, V]]]].compare(xfactors, yfactors)
            case _ => 1
          }
        }
      }
    }
  }

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
    override lazy val hashCode = (r, k).hashCode
  }
  case class sum[A: Ordering, V: Ordering](terms: RationalExpression[A, V]*) extends RationalExpression[A, V] {
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
      } else if (newTerms.size == 1) {
        newTerms.head
      } else {
        sum(newTerms.sorted: _*)
      }
    }
    override lazy val hashCode = (1, terms).hashCode
  }
  case class product[A: Ordering, V: Ordering](factors: RationalExpression[A, V]*) extends RationalExpression[A, V] {
    override def variables = factors.flatMap(_.variables).toSet
    override def simplify(implicit field: Field[A]) = {
      val flatten = factors.flatMap({
        case product(subfactors @ _*) => subfactors
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
      } else if (newFactors.size == 1) {
        newFactors.head
      } else {
        product(newFactors.sorted: _*)
      }
    }
    override lazy val hashCode = (1, factors).hashCode
  }

  implicit def liftConstant[A, V](a: A): RationalExpression[A, V] = constant(a)
  implicit def liftVariable[A, V](v: V): RationalExpression[A, V] = variable(v)
  implicit def liftFraction[A: GCDRing, V](a: A): RationalExpression[Fraction[A], V] = constant(a)
  implicit def liftInt[A: GCDRing, V](i: Int): RationalExpression[Fraction[A], V] = constant(implicitly[GCDRing[A]].fromInt(i))

  implicit def fieldOfRationalExpressions[A: Field: Ordering, V: Ordering]: Field[RationalExpression[A, V]] = new Field[RationalExpression[A, V]] {
    private def field = implicitly[Field[A]]
    override val zero = RationalExpression.constant[A, V](field.zero)
    override val one = RationalExpression.constant[A, V](field.one)
    override val negativeOne = RationalExpression.constant[A, V](field.negativeOne)
    override def fromInt(i: Int) = RationalExpression.constant[A, V](field.fromInt(i))
    override def fromBigInt(i: BigInt) = RationalExpression.constant[A, V](field.fromBigInt(i))
    override def fromBigInteger(i: java.math.BigInteger) = RationalExpression.constant[A, V](field.fromBigInteger(i))
    override def negate(x: RationalExpression[A, V]) = RationalExpression.product(RationalExpression.constant[A, V](field.negativeOne), x).simplify
    override def inverse(x: RationalExpression[A, V]) = power[Int](x, -1).simplify
    override def add(x: RationalExpression[A, V], y: RationalExpression[A, V]) = RationalExpression.sum(x, y).simplify
    override def multiply(x: RationalExpression[A, V], y: RationalExpression[A, V]) = RationalExpression.product(x, y).simplify
    override def power[I: IntegerModel](x: RationalExpression[A, V], k: I) = RationalExpression.power(x, Integers.from(k)).simplify
  }
}