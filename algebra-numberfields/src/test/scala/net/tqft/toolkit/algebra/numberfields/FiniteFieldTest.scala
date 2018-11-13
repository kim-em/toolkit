package net.tqft.toolkit.algebra.numberfields

import org.scalatest._

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials.Polynomial

class FiniteFieldTest extends FlatSpec with Matchers {

  "Finite field arithmetic" should "be correct" in {
    val ff = FiniteField(7, 2)

    val q: Polynomial[Int] = Polynomial(0 -> 3, 1 -> 5)

    ff.subtract(ff.multiply(q, q), ff.multiply(ff.add(q, ff.one), ff.subtract(q, ff.one))) should equal(Polynomial(0 -> 1))
  }
}
