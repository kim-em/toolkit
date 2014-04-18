package net.tqft.toolkit.algebra.spiders.examples

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.spiders._

@RunWith(classOf[JUnitRunner])
class G2SpiderTest extends FlatSpec with Matchers with IsomorphismMatchers {

  "G2" should "automatically compute the relation reducing a pentagon" in {

      val preG2 = {
        val omega: RationalFunction[BigInt] = 1
        val d: RationalFunction[BigInt] = Fraction(Seq(1,1,0,0,1,1,1,0,0,1,1), Seq(0,0,0,0,0,1))
        val b: RationalFunction[BigInt] = Fraction(Seq(1,1,1,0,1,1,1), Seq(0,0,0,1))
        val t: RationalFunction[BigInt] = Fraction(Seq(-1,0,-1,0,-1), Seq(0,0,1))
//        val d: RationalFunction[BigInt] = Fraction(Map(10 -> 1, 9 -> 1, 6 -> 1, 5 -> 1, 4 -> 1, 1 -> 1, 0 -> 1), Map(5 -> 1))
//        val b: RationalFunction[BigInt] = Fraction(Map(6 -> 1, 5 -> 1, 4 -> 1, 2 -> 1, 1 -> 1, 0 -> 1), Map(3 -> 1))
//        val t: RationalFunction[BigInt] = Fraction(Map(4 -> -1, 2 -> -1, 0 -> -1), Map(2 -> 1))
        
        Spiders.cubic(omega, d, b, t)
      }
      println(preG2.reductions)
      val basis = preG2.basis(5, preG2.reducedDiagrams(5, 1) ++ preG2.reducedDiagrams(5, 3))
      for (r <- basis.deriveNewRelations(5)) {
        println(r)
      }
      
      `(G_2)_q`
  }

}