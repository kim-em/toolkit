package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._

@RunWith(classOf[JUnitRunner])
class G2SpiderTest extends FlatSpec with Matchers with IsomorphismMatchers {

//  object preCubic extends MultivariableRationalFunctionTrivalentSpider[BigInt] {
//    override def coefficientRing = implicitly[EuclideanRing[BigInt]]
//    override val omega = ring.one
//  }
//  "PreCubic" should "automatically compute the relation reducing a square" in {
//    val basis = preCubic.reducedDiagrams(4, 0) ++ preCubic.reducedDiagrams(4, 2)
//    for (r <- preCubic.basis(4, basis).deriveNewRelations(4)) {
//      println(r)
//    }
//  }
  object preG2 extends TrivalentSpider[RationalFunction[Int]] {
    override def ring = implicitly[GCDRing[RationalFunction[Int]]]
    override val omega = ring.one
    override val d: RationalFunction[Int] = Map(5 -> 1, 4 -> 1, 1 -> 1, 0 -> 1, -1 -> 1, -4 -> 1, -5 -> 1)
    override val b: RationalFunction[Int] = Map(3 -> 1, 2 -> 1, 1 -> 1, -1 -> 1, -2 -> 1, -3 -> 1)
    override val t: RationalFunction[Int] = Map(2 -> -1, 0 -> -1, -2 -> -1)

  }
  "G2" should "automatically compute the relation reducing a pentagon" in {
    val basis = preG2.reducedDiagrams(5, 1) ++ preG2.reducedDiagrams(5, 3)
    for (r <- preG2.basis(5, basis).deriveNewRelations(5)) {
      println(r)
    }
  }

}