package net.tqft.toolkit.algebra.combinatorics

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.matrices.Matrices
import net.tqft.toolkit.algebra.matrices.Matrix

@RunWith(classOf[JUnitRunner])
class MatrixDecompositionTest extends FlatSpec with Matchers {

  "PositiveSymmetricDecompositions" should "should find all decompositions M=AA^t" in {
    PositiveSymmetricDecompositions(Matrix(1, List(List(5)))).toList.size should equal(2)

    PositiveSymmetricDecompositions(Matrix(2, List(List(1, 1), List(1, 1)))).toList.size should equal(1)

    PositiveSymmetricDecompositions(Matrix(2, List(List(1, 2), List(2, 4)))).toList.size should equal(1)

    PositiveSymmetricDecompositions(Matrix(10, Seq(
      List(3, 3, 1, 0, 1, 3, 3, 3, 2, 6),
      List(3, 3, 1, 0, 1, 3, 3, 3, 2, 6),
      List(1, 1, 4, 4, 2, 3, 3, 3, 3, 4),
      List(0, 0, 4, 6, 1, 3, 3, 3, 5, 3),
      List(1, 1, 2, 1, 8, 8, 8, 8, 7, 9),
      List(3, 3, 3, 3, 8, 11, 11, 11, 11, 14),
      List(3, 3, 3, 3, 8, 11, 11, 11, 11, 14),
      List(3, 3, 3, 3, 8, 11, 11, 11, 11, 14),
      List(2, 2, 3, 5, 7, 11, 11, 11, 13, 13),
      List(6, 6, 4, 3, 9, 14, 14, 14, 13, 20)))).toList.size should equal(2)

    println(PositiveSymmetricDecompositions(Matrix(2, List(List(6, 7), List(7, 14)))).toList.mkString("\n\n"))

    PositiveSymmetricDecompositions(Matrix(2, List(List(6, 7), List(7, 14)))).toList.size should equal(11)
    PositiveSymmetricDecompositions(Matrix(3, List(List(2, 2, 1), List(2, 2, 1), List(1, 1, 2)))).toList.size should equal(1)
    PositiveSymmetricDecompositions(Matrix(4, List(List(1, 1, 0, 0), List(1, 3, 2, 1), List(0, 2, 2, 1), List(0, 1, 1, 2)))).toList.size should equal(1)

    //    PositiveSymmetricDecompositions.cached(Matrix(4, List(List(1, 1, 0, 0), List(1, 3, 2, 1), List(0, 2, 2, 1), List(0, 1, 1, 2)))).toList.size should equal(1)

  }

  "choleskyDecomposition" should "find the LL^t decomposition" in {
    import net.tqft.toolkit.algebra.matrices.CholeskyDecomposition._
    
    (List(List(5.0, 2.0), List(2.0, 5.0)): Matrix[Double]).choleskyDecomposition should be('nonEmpty)
    (List(List(2, 2, 1), List(2, 2, 1), List(1, 1, 2)): Matrix[Int]).mapEntries(x => x: Double).choleskyDecomposition should be('nonEmpty)

    //    println((List(List(2,2,1), List(2,2,1), List(1,1,2)): Matrix[Int]).mapEntries(x => x: Double).choleskyDecomposition.get)
    (List(List(1, 2, 3, 2, 3, 5), List(2, 6, 8, 8, 12, 18), List(3, 8, 11, 10, 15, 23), List(2, 8, 10, 14, 20, 28), List(3, 12, 15, 20, 29, 41), List(5, 18, 23, 28, 41, 59)): Matrix[Int]).mapEntries(x => x: Double).choleskyDecomposition should be('nonEmpty)
    //    println((List(List(1,2,3,2,3,5), List(2,6,8,8,12,18), List(3,8,11,10,15,23), List(2,8,10,14,20,28),List(3,12,15,20,29,41), List(5,18,23,28,41,59)): Matrix[Int]).mapEntries(x => x: Double).choleskyDecomposition.get)
    (List(List(1.0, 2.0), List(2.0, 1.0)): Matrix[Double]).choleskyDecomposition should be('empty)
  }
}
