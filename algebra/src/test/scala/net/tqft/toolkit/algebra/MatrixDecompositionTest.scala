package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.Implicits.Integers;
import net.tqft.toolkit.algebra.Implicits.Rationals;
import net.tqft.toolkit.algebra.Implicits.integersAsRationals;

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixDecompositionTest extends FlatSpec with ShouldMatchers {

  "positiveSymmetricDecompositions" should "should find all decompositions M=AA^t" in {
    val m: Matrix[Int] = Matrix(2, List(List(2,1), List(1,2)))

    println(Matrices.positiveSymmetricDecompositions(m).toList)
  }
}
