package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.AssociativeAlgebra
import scala.language.implicitConversions

trait PowerSeries[A] {
  def coefficientOf(k: Int)(implicit zero: Zero[A]): A = {
    nonzeroCoefficients.find(p => p._1 >= k) match {
      case None => zero.zero
      case Some((m, a)) => {
        if (m == k) {
          a
        } else {
          zero.zero
        }
      }
    }
  }
  def coefficients: Stream[A]
  def nonzeroCoefficients(implicit zero: Zero[A]): Stream[(Int, A)]
}

case class DensePowerSeries[A](override val coefficients: Stream[A]) extends PowerSeries[A] {
  override def nonzeroCoefficients(implicit zero: Zero[A]) = coefficients.zipWithIndex.map(_.swap).filter(_._2 != zero.zero)
}

object PowerSeries {
  implicit def fromStream[A](stream: Stream[A]): PowerSeries[A] = DensePowerSeries(stream)
}

trait PowerSeriesAlgebra[A] extends AssociativeAlgebra[A, PowerSeries[A]] {
  val ring: Ring[A]

  override def zero = Stream.empty[A]
  override def one = Stream(ring.one)
  override def fromInt(k: Int) = Stream(ring.fromInt(k))
  override def negate(x: PowerSeries[A]) = x.coefficients.map(ring.negate)
  override def scalarMultiply(a: A, b: PowerSeries[A]) = b.coefficients.map(x => ring.multiply(a, x))
  override def add(x: PowerSeries[A], y: PowerSeries[A]) = {
    x.coefficients.zipAll(y.coefficients, ring.zero, ring.zero).map(p => ring.add(p._1, p._2))
  }
  override def multiply(x: PowerSeries[A], y: PowerSeries[A]) = {
    ???
  }
}

object PowerSeriesAlgebra {
  implicit def over[A: Ring]: PowerSeriesAlgebra[A] = new PowerSeriesAlgebra[A] {
	  override val ring = implicitly[Ring[A]]
  }
}