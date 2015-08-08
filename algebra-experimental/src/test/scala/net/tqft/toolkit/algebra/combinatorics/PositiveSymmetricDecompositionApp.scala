package net.tqft.toolkit.algebra.combinatorics

/**
 * @author scott
 */
object PositiveSymmetricDecompositionApp extends App {
  val n = math.sqrt(args.size).toInt
  val M = args.map(_.toInt).grouped(n).toArray
  
  for(A <- PositiveSymmetricDecomposition(M).decompositions) {
    println(A.map(_.mkString(" ")).mkString("\n"))
    println
  }
}