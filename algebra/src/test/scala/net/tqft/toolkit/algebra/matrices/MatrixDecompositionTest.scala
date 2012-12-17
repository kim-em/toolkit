package net.tqft.toolkit.algebra.matrices

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.Conversions

@RunWith(classOf[JUnitRunner])
class MatrixDecompositionTest extends FlatSpec with ShouldMatchers {

  "positiveSymmetricDecompositions" should "should find all decompositions M=AA^t" in {
    Matrices.positiveSymmetricDecompositions(Matrix(2, List(List(1, 1), List(1, 1)))).toList.size should equal(1)

    println(Matrices.positiveSymmetricDecompositions(Matrix(1, List(List(5)))).toList.mkString("\n\n"))

    Matrices.positiveSymmetricDecompositions(Matrix(1, List(List(5)))).toList.size should equal(2)
    Matrices.positiveSymmetricDecompositions(Matrix(2, List(List(1, 2), List(2, 4)))).toList.size should equal(1)

    println(Matrices.positiveSymmetricDecompositions(Matrix(2, List(List(6, 7), List(7, 14)))).toList.mkString("\n\n"))

    Matrices.positiveSymmetricDecompositions(Matrix(2, List(List(6, 7), List(7, 14)))).toList.size should equal(11)
    Matrices.positiveSymmetricDecompositions(Matrix(3, List(List(2, 2, 1), List(2, 2, 1), List(1, 1, 2)))).toList.size should equal(1)
    Matrices.positiveSymmetricDecompositions(Matrix(4, List(List(1, 1, 0, 0), List(1, 3, 2, 1), List(0, 2, 2, 1), List(0, 1, 1, 2)))).toList.size should equal(1)

    Matrices.positiveSymmetricDecompositionsCached(Matrix(4, List(List(1, 1, 0, 0), List(1, 3, 2, 1), List(0, 2, 2, 1), List(0, 1, 1, 2)))).toList.size should equal(1)

  }

  "choleskyDecomposition" should "find the LL^t decomposition" in {
    (List(List(5.0, 2.0), List(2.0, 5.0)): Matrix[Double]).choleskyDecomposition should be('nonEmpty)
    (List(List(2,2,1), List(2,2,1), List(1,1,2)): Matrix[Int]).mapEntries(Conversions.integersAsDoubles).choleskyDecomposition should be('nonEmpty)
    
//    println((List(List(2,2,1), List(2,2,1), List(1,1,2)): Matrix[Int]).mapEntries(Conversions.integersAsDoubles).choleskyDecomposition.get)
    (List(List(1,2,3,2,3,5), List(2,6,8,8,12,18), List(3,8,11,10,15,23), List(2,8,10,14,20,28),List(3,12,15,20,29,41), List(5,18,23,28,41,59)): Matrix[Int]).mapEntries(Conversions.integersAsDoubles).choleskyDecomposition should be('nonEmpty)
//    println((List(List(1,2,3,2,3,5), List(2,6,8,8,12,18), List(3,8,11,10,15,23), List(2,8,10,14,20,28),List(3,12,15,20,29,41), List(5,18,23,28,41,59)): Matrix[Int]).mapEntries(Conversions.integersAsDoubles).choleskyDecomposition.get)
    (List(List(1.0, 2.0), List(2.0, 1.0)): Matrix[Double]).choleskyDecomposition should be('empty)
  }
}
