package net.tqft.toolkit.algebra.polynomials

import net.tqft.toolkit.algebra._
import org.scalatest._

class FactoringTest extends FlatSpec with Matchers {
  val p = implicitly[PolynomialsOverIntegerModel[BigInt]]
  val x: Polynomial[BigInt] = Polynomial(4 -> 1, 0 -> 100)
  "ZassenhausFactoring" should "correctly factorize x^4 - 100" in {
    //  We don't need to 'import ZassenhausFactoring._', because PolynomialsOverIntegerModel provides it as the default implementation.
    p.factor(x) should equal(null)
  }
  "LLLFactoring" should "correctly factorize x^4 - 100" in {
    import LLLFactoring._
    p.factor(x) should equal(null)
  }
}