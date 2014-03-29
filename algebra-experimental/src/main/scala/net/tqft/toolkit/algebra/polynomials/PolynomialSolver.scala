package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra.OrderedField

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
    def evaluateAt(x: D): D = field.sum(p.coefficients.toSeq.map({ case (e, a) => field.multiply(a, field.power(x, e)) }))
    
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