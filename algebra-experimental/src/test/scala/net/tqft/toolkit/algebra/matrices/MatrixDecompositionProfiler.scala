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
// extended Haagerup; might take 2 years?
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

    val M4red = Array(
 Array(6, 6, 3, 5, 9, 13),
 Array(6, 8, 3, 5, 8, 13),
 Array(3, 3, 15, 11, 30, 48),
 Array(5, 5, 11, 17, 29, 45),
 Array(9, 8, 30, 29, 79, 115),
 Array(13, 13, 48, 45, 115, 181))

    
// a day or two?
val M_AH = Seq(
List(5, 2, 1, 5, 3, 3, 5, 4, 8, 6, 7, 8, 7, 10, 10), 
List(2, 6, 6, 5, 3, 3, 5, 5, 5, 5, 8, 8, 8, 8, 11), 
List(1, 6, 9, 5, 3, 3, 5, 8, 4, 6, 7, 8, 11, 6, 10), 
List(5, 5, 5, 16, 12, 12, 16, 16, 17, 18, 29, 29, 29, 30, 41), 
List(3, 3, 3, 12, 16, 16, 12, 12, 19, 18, 27, 27, 27, 34, 43), 
List(3, 3, 3, 12, 16, 16, 12, 12, 19, 18, 27, 27, 27, 34, 43), 
List(5, 5, 5, 16, 12, 12, 16, 16, 17, 18, 29, 29, 29, 30, 41), 
List(4, 5, 8, 16, 12, 12, 16, 19, 16, 19, 28, 29, 32, 28, 40), 
List(8, 5, 4, 17, 19, 19, 17, 16, 27, 24, 34, 35, 34, 44, 53), 
List(6, 5, 6, 18, 18, 18, 18, 19, 24, 35, 46, 45, 46, 52, 64), 
List(7, 8, 7, 29, 27, 27, 29, 28, 34, 46, 69, 66, 65, 74, 96), 
List(8, 8, 8, 29, 27, 27, 29, 29, 35, 45, 66, 70, 70, 72, 93), 
List(7, 8, 11, 29, 27, 27, 29, 32, 34, 46, 65, 70, 73, 70, 92), 
List(10, 8, 6, 30, 34, 34, 30, 28, 44, 52, 74, 72, 70, 88, 108), 
List(10, 11, 10, 41, 43, 43, 41, 40, 53, 64, 96, 93, 92, 108, 139)
)
    
  def M(k: Int) = Matrix(k, M4.take(k).map(_.take(k)))
  def Ma(k: Int) = M4.take(k).map(_.take(k).toArray).toArray

//  for(t <- Profiler.movingTimingAverages(3)(Matrices.positiveSymmetricDecompositions(M(6)).size)) {
//    println(t)
//  }
//  for(k <- 1 to 10) { println(PositiveSymmetricDecomposition(Ma(k)).decompositions.size) }
  for(t <- PositiveSymmetricDecomposition(M4red).runtimeEstimators) println(t)
//  for(t <- PositiveSymmetricDecomposition(Ma(14)).numberOfDescendantsEstimators) println(t)
//  for(p <- Iterator.continually(PositiveSymmetricDecomposition(Ma(14)).randomPartialSolution)) println(p.length)
  
//  println(PositiveSymmetricDecomposition(Ma(14)).numberOfDecompositionsEstimators.toStream.apply(20))
  
//  for(n <- PositiveSymmetricDecomposition(Ma(14)).numberOfDecompositionsEstimators) println(n)
  
  for(p <- Iterator.continually(PositiveSymmetricDecomposition(Ma(14)).randomPartialSolution); if p.length == 14) {
    println
    for(r <- p) println(r.toList)
  }
//  for(k <- 1 to 14; t <- PositiveSymmetricDecomposition(Ma(k)).runtimeEstimators.take(50)) println(k + " " + t)
  for(m <- PositiveSymmetricDecomposition(Ma(14)).decompositions) { println(m) }
}