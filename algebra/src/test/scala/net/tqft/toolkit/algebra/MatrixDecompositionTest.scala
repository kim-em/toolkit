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
    Matrices.positiveSymmetricDecompositions(Matrix(2, List(List(1,1), List(1,1)))).toList.size should equal(1)
    
    println(Matrices.positiveSymmetricDecompositions(Matrix(1, List(List(5)))).toList.mkString("\n\n"))

    Matrices.positiveSymmetricDecompositions(Matrix(1, List(List(5)))).toList.size should equal(2)
    Matrices.positiveSymmetricDecompositions(Matrix(2, List(List(1,2),List(2,4)))).toList.size should equal(1)
    
    println(Matrices.positiveSymmetricDecompositions(Matrix(2, List(List(6,7),List(7,14)))).toList.mkString("\n\n"))
    
    Matrices.positiveSymmetricDecompositions(Matrix(2, List(List(6,7),List(7,14)))).toList.size should equal(11)
    Matrices.positiveSymmetricDecompositions(Matrix(3, List(List(2,2,1),List(2,2,1),List(1,1,2)))).toList.size should equal(1)
    Matrices.positiveSymmetricDecompositions(Matrix(4, List(List(1,1,0,0),List(1,3,2,1),List(0,2,2,1),List(0,1,1,2)))).toList.size should equal(1)
  }
}
