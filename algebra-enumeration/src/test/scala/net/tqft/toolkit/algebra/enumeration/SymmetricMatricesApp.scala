package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues

/**
 * @author scott
 */
object SymmetricMatricesApp extends App {
  
  private def convertLowerTriangularToSquare(m: List[List[Int]]) = m.zipWithIndex map { case (row, index) => row ::: (m.drop(index + 1) map { _(index) }) }
  private def unreduce(m: List[List[Int]]): List[List[Int]] = {
    (0 :: 1 :: List.fill(m.size - 1)(0)) ::
    (1 :: m.head) ::
    m.tail.map(0 :: _)
  }
  
  
  def limit(D: Double)(m: List[List[Int]]) = {
    FrobeniusPerronEigenvalues.estimate(unreduce(convertLowerTriangularToSquare(m)).toArray.map(_.toArray)) <= D
  }
  
  val k = 4
  
  val zero = List.tabulate(k)(i => List.fill(i + 1)(0))
  println(Odometer(limit(8))(zero).size)
}