package net.tqft.toolkit.algebra.combinatorics

import java.io.File
import java.io.FileOutputStream
import java.io.PrintWriter

/**
 * @author scott
 */
object PositiveSymmetricDecompositionApp extends App {
  
  val (out, matrix) = if(args.head == "-o") {
    (new PrintWriter(new FileOutputStream(new File(args(1)))), args.tail.tail)
  } else {
    (new PrintWriter(System.out), args)
  }
  
  val n = math.sqrt(matrix.size).toInt
  val M = matrix.map(_.toInt).grouped(n).toArray
  
  for(A <- PositiveSymmetricDecomposition(M).decompositions) {
    out.println(A.map(_.mkString(" ")).mkString("\n"))
    out.println
  }
  
  out.flush
}