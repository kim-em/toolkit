package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.numberfields.NumberField
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.matrices.Matrices
import net.tqft.toolkit.algebra.spiders.examples.CubicSpider




object Spiders {
  def cubic[R: Field](omega: R, d: R, b: R, t: R) = {
    val omega_ = omega
    val d_ = d
    val b_ = b
    val t_ = t
    new CubicSpider[R] {
      override def ring = implicitly[Field[R]]
      override def omega = omega_
      override def d = d_
      override def b = b_
      override def t = t_
    }
  }
}






