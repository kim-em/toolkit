package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class ZassenhausFactoringTest extends FlatSpec with Matchers {
  val p = implicitly[PolynomialsOverIntegerModel[BigInt]]
  val x: Polynomial[BigInt] = ???
  import ZassenhausFactoring._
  p.factor(x) should equal(null)
}