package net.tqft.toolkit.algebra.matrices

import net.tqft.toolkit.Profiler

object MatrixDecompositionProfiler extends App {

  val M0 = Seq(
List(3, 3, 1, 0, 1, 3, 3, 3, 2, 6), 
List(3, 3, 1, 0, 1, 3, 3, 3, 2, 6), 
List(1, 1, 4, 4, 2, 3, 3, 3, 3, 4), 
List(0, 0, 4, 6, 1, 3, 3, 3, 5, 3), 
List(1, 1, 2, 1, 8, 8, 8, 8, 7, 9), 
List(3, 3, 3, 3, 8, 11, 11, 11, 11, 14), 
List(3, 3, 3, 3, 8, 11, 11, 11, 11, 14), 
List(3, 3, 3, 3, 8, 11, 11, 11, 11, 14), 
List(2, 2, 3, 5, 7, 11, 11, 11, 13, 13), 
List(6, 6, 4, 3, 9, 14, 14, 14, 13, 20)
)
  val M4 = Seq(
    List(6, 6, 3, 3, 5, 5, 8, 8, 9, 12, 12, 13, 13, 15),
    List(6, 8, 3, 3, 5, 7, 8, 6, 8, 13, 13, 13, 11, 16),
    List(3, 3, 15, 15, 11, 11, 26, 26, 30, 45, 45, 48, 48, 60),
    List(3, 3, 15, 15, 11, 11, 26, 26, 30, 45, 45, 48, 48, 60),
    List(5, 5, 11, 11, 17, 17, 28, 28, 29, 40, 40, 45, 45, 51),
    List(5, 7, 11, 11, 17, 19, 28, 26, 28, 41, 41, 45, 43, 52),
    List(8, 8, 26, 26, 28, 28, 54, 54, 59, 85, 85, 93, 93, 111),
    List(8, 6, 26, 26, 28, 26, 54, 56, 60, 84, 84, 93, 95, 110),
    List(9, 8, 30, 30, 29, 28, 59, 60, 79, 108, 108, 115, 116, 138),
    List(12, 13, 45, 45, 40, 41, 85, 84, 108, 154, 154, 163, 162, 199),
    List(12, 13, 45, 45, 40, 41, 85, 84, 108, 154, 154, 163, 162, 199),
    List(13, 13, 48, 48, 45, 45, 93, 93, 115, 163, 163, 181, 181, 211),
    List(13, 11, 48, 48, 45, 43, 93, 95, 116, 162, 162, 181, 183, 210),
    List(15, 16, 60, 60, 51, 52, 111, 110, 138, 199, 199, 211, 210, 259))

  def M(k: Int) = Matrix(k, M0.take(k).map(_.take(k)))

//  for(t <- Profiler.movingTimingAverages(3)(Matrices.positiveSymmetricDecompositions(M(6)).size)) {
//    println(t)
//  }
//  for(k <- 1 to 7) { println(Matrices.positiveSymmetricDecompositions(M(k)).size) }
  for(m <- Matrices.positiveSymmetricDecompositions(M(10))) { println(m) }
}