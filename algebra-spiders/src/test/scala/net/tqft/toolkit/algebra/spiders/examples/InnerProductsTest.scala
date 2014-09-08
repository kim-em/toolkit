package net.tqft.toolkit.algebra.spiders.examples

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.mathematica.MathematicaForm

@RunWith(classOf[JUnitRunner])
class InnerProductsTest extends FlatSpec with Matchers with IsomorphismMatchers {

  val `D(4,0)` = TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(4, 0).toList

//  "inner products of D(4,0)" should "be correct" in {
//    val spider = TrivalentSpider
//    val `M(4,0)` = spider.innerProductMatrix(`D(4,0)`)
//
//    val d = "d"
//    val b = "b"
//    val t = "t"
//    val rationalResult: List[List[MultivariablePolynomial[Fraction[Int], String]]] = List(List(MultivariablePolynomial(Map(Map(d -> 2) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map()), MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> Fraction(1, 1)))), List(MultivariablePolynomial(Map(Map(d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(d -> 2) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map())), List(MultivariablePolynomial(Map()), MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(b -> 2, d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(t -> 1, b -> 1, d -> 1) -> Fraction(1, 1)))), List(MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map()), MultivariablePolynomial(Map(Map(t -> 1, b -> 1, d -> 1) -> Fraction(1, 1))), MultivariablePolynomial(Map(Map(b -> 2, d -> 1) -> Fraction(1, 1)))))
//    val integerResult: List[List[MultivariablePolynomial[Int, String]]] = List(List(MultivariablePolynomial(Map(Map(d -> 2) -> 1)), MultivariablePolynomial(Map(Map(d -> 1) -> 1)), MultivariablePolynomial(Map()), MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> 1))), List(MultivariablePolynomial(Map(Map(d -> 1) -> 1)), MultivariablePolynomial(Map(Map(d -> 2) -> 1)), MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> 1)), MultivariablePolynomial(Map())), List(MultivariablePolynomial(Map()), MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> 1)), MultivariablePolynomial(Map(Map(b -> 2, d -> 1) -> 1)), MultivariablePolynomial(Map(Map(t -> 1, b -> 1, d -> 1) -> 1))), List(MultivariablePolynomial(Map(Map(b -> 1, d -> 1) -> 1)), MultivariablePolynomial(Map()), MultivariablePolynomial(Map(Map(t -> 1, b -> 1, d -> 1) -> 1)), MultivariablePolynomial(Map(Map(b -> 2, d -> 1) -> 1))))
//
//    `M(4,0)` should equal(integerResult)
//  }

}

